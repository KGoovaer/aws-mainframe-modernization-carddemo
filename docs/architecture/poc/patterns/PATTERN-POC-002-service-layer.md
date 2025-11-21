# PATTERN-POC-002: Service Layer Pattern

**Status**: Active  
**Category**: Business Logic  
**Complexity**: Low  
**Last Updated**: 2025-11-20

## Context

The POC needs a place to put business logic that is separate from controllers (API) and pages (Angular UI), while keeping it simple and easy to test.

## Problem

- Controllers should be thin (handle HTTP concerns only)
- Business logic should not be duplicated across controllers and pages
- Need to test business logic without HTTP context
- Want to enforce business rules consistently
- Must coordinate multiple repositories for complex operations

## Solution

Use a **Service Layer** to encapsulate business logic.

### Key Characteristics (POC)
- ✅ One service class per business domain (AccountService, CardService, etc.)
- ✅ Services depend on repositories, not DbContext
- ✅ Services are stateless (no instance fields except dependencies)
- ✅ Async methods throughout
- ✅ Simple transaction handling via DbContext

## Implementation

### 1. Service Structure

```csharp
public class AccountService
{
    private readonly IAccountRepository _accountRepository;
    private readonly ICustomerRepository _customerRepository;
    private readonly ILogger<AccountService> _logger;
    
    public AccountService(
        IAccountRepository accountRepository,
        ICustomerRepository customerRepository,
        ILogger<AccountService> logger)
    {
        _accountRepository = accountRepository;
        _customerRepository = customerRepository;
        _logger = logger;
    }
    
    // Business methods here
}
```

### 2. Read Operations (Queries)

```csharp
public async Task<AccountDetailsResponse> GetAccountDetailsAsync(int accountId)
{
    _logger.LogInformation("Getting account details for account {AccountId}", accountId);
    
    var account = await _accountRepository.GetByIdAsync(accountId);
    
    if (account == null)
    {
        _logger.LogWarning("Account {AccountId} not found", accountId);
        throw new NotFoundException($"Account {accountId} not found");
    }
    
    // Business logic: Calculate available credit
    var availableCredit = account.CreditLimit - account.CurrentBalance;
    
    return new AccountDetailsResponse
    {
        AccountId = account.Id,
        AccountNumber = account.AccountNumber,
        CustomerName = $"{account.Customer.FirstName} {account.Customer.LastName}",
        CurrentBalance = account.CurrentBalance,
        CreditLimit = account.CreditLimit,
        AvailableCredit = availableCredit,
        AccountStatus = account.AccountStatus,
        OpenDate = account.OpenDate,
        ExpiryDate = account.ExpiryDate
    };
}
```

### 3. Write Operations (Commands)

```csharp
public async Task<Account> UpdateAccountAsync(int accountId, UpdateAccountRequest request)
{
    _logger.LogInformation("Updating account {AccountId}", accountId);
    
    // Retrieve account
    var account = await _accountRepository.GetByIdAsync(accountId);
    if (account == null)
        throw new NotFoundException($"Account {accountId} not found");
    
    // Business Rule 1: Cannot increase credit limit for closed accounts
    if (account.AccountStatus == "C" && request.CreditLimit > account.CreditLimit)
    {
        throw new BusinessRuleException("Cannot increase credit limit for closed accounts");
    }
    
    // Business Rule 2: Credit limit must be >= current balance
    if (request.CreditLimit < account.CurrentBalance)
    {
        throw new BusinessRuleException("Credit limit cannot be less than current balance");
    }
    
    // Business Rule 3: Validate account status
    var validStatuses = new[] { "A", "C", "S" }; // Active, Closed, Suspended
    if (!validStatuses.Contains(request.AccountStatus))
    {
        throw new ValidationException("Invalid account status");
    }
    
    // Apply updates
    account.CreditLimit = request.CreditLimit;
    account.AccountStatus = request.AccountStatus;
    
    // Save changes
    await _accountRepository.UpdateAsync(account);
    
    _logger.LogInformation("Account {AccountId} updated successfully", accountId);
    
    return account;
}
```

### 4. Complex Operations (Multi-Entity Transactions)

```csharp
public async Task<Transaction> AddTransactionAsync(AddTransactionRequest request)
{
    _logger.LogInformation("Adding transaction for account {AccountNumber}", request.AccountNumber);
    
    // Step 1: Retrieve account
    var account = await _accountRepository.GetByAccountNumberAsync(request.AccountNumber);
    if (account == null)
        throw new NotFoundException($"Account {request.AccountNumber} not found");
    
    // Step 2: Retrieve card
    var card = await _cardRepository.GetByCardNumberAsync(request.CardNumber);
    if (card == null)
        throw new NotFoundException($"Card {request.CardNumber} not found");
    
    // Business Rule 1: Card must belong to account
    if (card.AccountId != account.Id)
        throw new BusinessRuleException("Card does not belong to account");
    
    // Business Rule 2: Card must be active
    if (card.CardStatus != "A")
        throw new BusinessRuleException("Card is not active");
    
    // Business Rule 3: Check credit limit for purchases
    if (request.TransactionType == "P") // Purchase
    {
        var newBalance = account.CurrentBalance + request.Amount;
        if (newBalance > account.CreditLimit)
            throw new BusinessRuleException("Transaction would exceed credit limit");
        
        // Update account balance
        account.CurrentBalance = newBalance;
        await _accountRepository.UpdateAsync(account);
    }
    else if (request.TransactionType == "R") // Payment
    {
        // Update account balance
        account.CurrentBalance -= request.Amount;
        if (account.CurrentBalance < 0)
            account.CurrentBalance = 0;
        
        await _accountRepository.UpdateAsync(account);
    }
    
    // Step 3: Create transaction record
    var transaction = new Transaction
    {
        AccountId = account.Id,
        CardNumber = card.CardNumber,
        TransactionType = request.TransactionType,
        TransactionCategory = request.TransactionCategory,
        TransactionAmount = request.Amount,
        TransactionDate = DateTime.UtcNow,
        TransactionDesc = request.Description,
        TransactionSource = "ONLINE",
        ProcessingStatus = "P" // Pending
    };
    
    await _transactionRepository.AddAsync(transaction);
    
    _logger.LogInformation("Transaction {TransactionId} created successfully", transaction.Id);
    
    return transaction;
}
```

### 5. Batch Operations

```csharp
public async Task<InterestCalculationResult> CalculateMonthlyInterestAsync()
{
    _logger.LogInformation("Starting monthly interest calculation");
    
    var result = new InterestCalculationResult
    {
        StartTime = DateTime.UtcNow,
        AccountsProcessed = 0,
        TotalInterest = 0,
        Errors = new List<string>()
    };
    
    // Get all active accounts
    var accounts = await _accountRepository.GetAllAsync();
    
    foreach (var account in accounts.Where(a => a.AccountStatus == "A"))
    {
        try
        {
            // Business logic: Calculate interest (from CBACT04C)
            // Annual rate = 12%, Monthly rate = 1%
            var monthlyRate = 0.01m;
            var interest = account.CurrentBalance * monthlyRate;
            
            if (interest > 0)
            {
                // Add interest to balance
                account.CurrentBalance += interest;
                await _accountRepository.UpdateAsync(account);
                
                // Create interest transaction
                var interestTxn = new Transaction
                {
                    AccountId = account.Id,
                    TransactionType = "I", // Interest
                    TransactionCategory = "99", // Interest category
                    TransactionAmount = interest,
                    TransactionDate = DateTime.UtcNow,
                    TransactionDesc = "Monthly interest charge",
                    TransactionSource = "BATCH",
                    ProcessingStatus = "C" // Complete
                };
                await _transactionRepository.AddAsync(interestTxn);
            }
            
            result.AccountsProcessed++;
            result.TotalInterest += interest;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error calculating interest for account {AccountId}", account.Id);
            result.Errors.Add($"Account {account.AccountNumber}: {ex.Message}");
        }
    }
    
    result.EndTime = DateTime.UtcNow;
    result.Duration = result.EndTime - result.StartTime;
    
    _logger.LogInformation("Interest calculation complete. Processed {Count} accounts, total interest: {Amount}",
        result.AccountsProcessed, result.TotalInterest);
    
    return result;
}
```

### 6. Controller Usage (API)

```csharp
[ApiController]
[Route("api/[controller]")]
public class AccountsController : ControllerBase
{
    private readonly AccountService _accountService;
    
    public AccountsController(AccountService accountService)
    {
        _accountService = accountService;
    }
    
    [HttpGet("{id}")]
    public async Task<IActionResult> GetAccount(int id)
    {
        try
        {
            var account = await _accountService.GetAccountDetailsAsync(id);
            return Ok(account);
        }
        catch (NotFoundException)
        {
            return NotFound();
        }
    }
    
    [HttpPut("{id}")]
    public async Task<IActionResult> UpdateAccount(int id, UpdateAccountRequest request)
    {
        try
        {
            var account = await _accountService.UpdateAccountAsync(id, request);
            return Ok(account);
        }
        catch (NotFoundException)
        {
            return NotFound();
        }
        catch (BusinessRuleException ex)
        {
            return BadRequest(new { error = ex.Message });
        }
    }
}
```

### 7. Angular Page Usage (UI)

```razor
@page "/accounts/{AccountId:int}"
@inject AccountService AccountService

<h1>Account Details</h1>

@if (account != null)
{
    <div>
        <p><strong>Account #:</strong> @account.AccountNumber</p>
        <p><strong>Customer:</strong> @account.CustomerName</p>
        <p><strong>Current Balance:</strong> @account.CurrentBalance.ToString("C")</p>
        <p><strong>Credit Limit:</strong> @account.CreditLimit.ToString("C")</p>
        <p><strong>Available Credit:</strong> @account.AvailableCredit.ToString("C")</p>
    </div>
}

@code {
    [Parameter]
    public int AccountId { get; set; }
    
    private AccountDetailsResponse account;
    
    protected override async Task OnInitializedAsync()
    {
        account = await AccountService.GetAccountDetailsAsync(AccountId);
    }
}
```

### 8. Dependency Injection Registration

```csharp
// In Program.java
builder.Services.AddScoped<AuthenticationService>();
builder.Services.AddScoped<AccountService>();
builder.Services.AddScoped<CardService>();
builder.Services.AddScoped<TransactionService>();
builder.Services.AddScoped<UserService>();
builder.Services.AddScoped<ReportService>();
```

## Testing

### Unit Test

```csharp
public class AccountServiceTests
{
    [Fact]
    public async Task UpdateAccountAsync_WhenCreditLimitBelowBalance_ThrowsBusinessRuleException()
    {
        // Arrange
        var mockAccountRepo = new Mock<IAccountRepository>();
        mockAccountRepo.Setup(r => r.GetByIdAsync(123, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Account
            {
                Id = 123,
                CurrentBalance = 5000.00m,
                CreditLimit = 10000.00m,
                AccountStatus = "A"
            });
        
        var service = new AccountService(mockAccountRepo.Object, null, Mock.Of<ILogger<AccountService>>());
        
        var request = new UpdateAccountRequest
        {
            CreditLimit = 4000.00m, // Less than current balance
            AccountStatus = "A"
        };
        
        // Act & Assert
        await Assert.ThrowsAsync<BusinessRuleException>(
            async () => await service.UpdateAccountAsync(123, request)
        );
    }
}
```

## Service Responsibilities

### What Services SHOULD Do
- ✅ Validate business rules
- ✅ Coordinate multiple repositories
- ✅ Implement business logic
- ✅ Map entities to DTOs
- ✅ Handle transactions
- ✅ Log operations
- ✅ Throw business exceptions

### What Services SHOULD NOT Do
- ❌ Handle HTTP concerns (status codes, headers)
- ❌ Directly access DbContext
- ❌ Contain UI logic
- ❌ Manage authentication/authorization
- ❌ Format output for display

## Consequences

### Advantages (✅)
- **Reusability**: Same service used by API and Angular pages
- **Testability**: Easy to unit test without HTTP or UI
- **Separation of Concerns**: Business logic isolated
- **Maintainability**: Changes to business logic in one place
- **Clear Intent**: Service method names describe business operations

### Disadvantages (⚠️)
- **Additional Layer**: More files and indirection
- **Potential Over-Engineering**: Simple CRUD may not need services

## When to Use (POC Context)

### Use Service Layer When:
- ✅ Business logic is non-trivial (validation, calculations, multi-step)
- ✅ Operation involves multiple repositories
- ✅ Logic is shared between API and UI
- ✅ Need to enforce business rules

### Skip Service Layer When:
- ❌ Simple CRUD with no business logic
- ❌ Single repository call with no validation

## COBOL to Service Mapping

| COBOL Program | Service | Key Methods |
|---------------|---------|-------------|
| COSGN00C | AuthenticationService | Login, Logout, ValidateSession |
| COACTVWC | AccountService | GetAccountDetails |
| COACTUPC | AccountService | UpdateAccount |
| CBACT04C | AccountService | CalculateMonthlyInterest |
| COCRDLIC | CardService | SearchCards |
| COCRDSLC | CardService | GetCardDetails |
| COCRDUPC | CardService | UpdateCard |
| COTRN00C | TransactionService | ListTransactions |
| COTRN02C | TransactionService | AddTransaction |
| CBTRN02C | TransactionService | PostTransactions (batch) |

## Related Patterns

- **Repository Pattern**: Services depend on repositories
- **DTO Pattern**: Services map entities to DTOs
- **CQRS**: NOT used in POC (final architecture will separate commands/queries)

## References

- Martin Fowler: [Service Layer](https://martinfowler.com/eaaCatalog/serviceLayer.html)
- Microsoft Docs: [Organizing Logic in ASPJava Spring Boot Core](https://learn.microsoft.com/en-us/aspnet/core/fundamentals/dependency-injection)
- Final Architecture: `../../patterns/PATTERN-002-application-services.md` (production version with CQRS)

---

**POC Reminder**: Services contain the business logic translated from COBOL. Keep them focused and testable.
