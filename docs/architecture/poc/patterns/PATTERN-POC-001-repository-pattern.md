# PATTERN-POC-001: Direct DbContext Access

**Status**: Active  
**Category**: Data Access  
**Complexity**: Very Low  
**Last Updated**: 2025-11-20

## Context

The POC needs the **simplest possible approach** to database access for rapid validation. Maximum simplicity is the priority.

## Problem

- Need to read/write data quickly
- Want minimal abstraction layers
- Must keep it extremely simple (POC is throwaway code)
- Repository pattern adds unnecessary complexity for POC

## Solution

Services **directly use DbContext** for all data access operations.

### Key Characteristics (POC)
- ✅ Services inject `CardDemoDbContext` directly
- ✅ Use EF Core LINQ queries in services
- ✅ No repository layer (removed for simplicity)
- ✅ No interfaces to mock (use in-memory database for tests)
- ✅ Absolute minimum code

## Implementation

### 1. Service with Direct DbContext Access

```csharp
public class AccountService
{
    private readonly CardDemoDbContext _context;
    private readonly ILogger<AccountService> _logger;
    
    public AccountService(
        CardDemoDbContext context,
        ILogger<AccountService> logger)
    {
        _context = context;
        _logger = logger;
    }
    
    public async Task<AccountDetailsResponse> GetAccountDetailsAsync(int accountId)
    {
        // Direct EF Core query in service
        var account = await _context.Accounts
            .Include(a => a.Customer)
            .Include(a => a.Cards)
            .FirstOrDefaultAsync(a => a.Id == accountId);
        
        if (account == null)
        {
            _logger.LogWarning("Account {AccountId} not found", accountId);
            throw new NotFoundException($"Account {accountId} not found");
        }
        
        return new AccountDetailsResponse
        {
            AccountNumber = account.AccountNumber,
            CustomerName = $"{account.Customer.FirstName} {account.Customer.LastName}",
            CurrentBalance = account.CurrentBalance,
            CreditLimit = account.CreditLimit,
            AccountStatus = account.AccountStatus
        };
    }
    
    public async Task<Account> UpdateAccountAsync(int accountId, UpdateAccountRequest request)
    {
        var account = await _context.Accounts.FindAsync(accountId);
        if (account == null)
            throw new NotFoundException($"Account {accountId} not found");
        
        // Business validation
        if (request.CreditLimit < account.CurrentBalance)
            throw new BusinessRuleException("Credit limit cannot be less than current balance");
        
        // Update entity
        account.CreditLimit = request.CreditLimit;
        account.AccountStatus = request.AccountStatus;
        
        // Save changes
        await _context.SaveChangesAsync();
        
        return account;
    }
    
    public async Task<Transaction> AddTransactionAsync(AddTransactionRequest request)
    {
        // Query account directly
        var account = await _context.Accounts
            .FirstOrDefaultAsync(a => a.AccountNumber == request.AccountNumber);
            
        if (account == null)
            throw new NotFoundException($"Account {request.AccountNumber} not found");
        
        // Business logic
        if (request.TransactionType == "P") // Purchase
        {
            var newBalance = account.CurrentBalance + request.Amount;
            if (newBalance > account.CreditLimit)
                throw new BusinessRuleException("Transaction would exceed credit limit");
            
            account.CurrentBalance = newBalance;
        }
        
        // Create transaction
        var transaction = new Transaction
        {
            AccountId = account.Id,
            TransactionType = request.TransactionType,
            TransactionAmount = request.Amount,
            TransactionDate = DateTime.UtcNow,
            ProcessingStatus = "P"
        };
        
        _context.Transactions.Add(transaction);
        await _context.SaveChangesAsync();
        
        return transaction;
    }
}
```

### 2. Dependency Injection Registration

```csharp
// In Program.java
builder.Services.AddDbContext<CardDemoDbContext>(options =>
    options.UseSqlite("Data Source=carddemo.db"));

// Register services (no repositories needed)
builder.Services.AddScoped<AccountService>();
builder.Services.AddScoped<CardService>();
builder.Services.AddScoped<TransactionService>();
builder.Services.AddScoped<AuthenticationService>();
```

## Testing

### Test with In-Memory Database

```csharp
public class AccountServiceTests
{
    private CardDemoDbContext CreateInMemoryContext()
    {
        var options = new DbContextOptionsBuilder<CardDemoDbContext>()
            .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
            .Options;
        
        return new CardDemoDbContext(options);
    }
    
    [Fact]
    public async Task GetAccountDetailsAsync_WithValidId_ReturnsAccountDetails()
    {
        // Arrange
        var context = CreateInMemoryContext();
        
        var customer = new Customer
        {
            FirstName = "John",
            LastName = "Doe"
        };
        context.Customers.Add(customer);
        
        var account = new Account
        {
            AccountNumber = 1000000001,
            CurrentBalance = 5000.00m,
            CreditLimit = 10000.00m,
            CustomerId = customer.Id,
            Customer = customer
        };
        context.Accounts.Add(account);
        await context.SaveChangesAsync();
        
        var service = new AccountService(context, Mock.Of<ILogger<AccountService>>());
        
        // Act
        var result = await service.GetAccountDetailsAsync(account.Id);
        
        // Assert
        result.Should().NotBeNull();
        result.AccountNumber.Should().Be(1000000001);
        result.CustomerName.Should().Be("John Doe");
        result.CurrentBalance.Should().Be(5000.00m);
    }
    
    [Fact]
    public async Task UpdateAccountAsync_WhenCreditLimitBelowBalance_ThrowsException()
    {
        // Arrange
        var context = CreateInMemoryContext();
        
        var account = new Account
        {
            AccountNumber = 1000000001,
            CurrentBalance = 5000.00m,
            CreditLimit = 10000.00m,
            AccountStatus = "A"
        };
        context.Accounts.Add(account);
        await context.SaveChangesAsync();
        
        var service = new AccountService(context, Mock.Of<ILogger<AccountService>>());
        var request = new UpdateAccountRequest
        {
            CreditLimit = 4000.00m, // Less than current balance
            AccountStatus = "A"
        };
        
        // Act & Assert
        await Assert.ThrowsAsync<BusinessRuleException>(
            async () => await service.UpdateAccountAsync(account.Id, request)
        );
    }
}
```

**Key Point**: Use EF Core In-Memory Database for testing. No mocking needed - test against real DbContext with in-memory provider.

## Consequences

### Advantages (✅)
- **Maximum Simplicity**: Fewest possible layers (just Service + DbContext)
- **Less Code**: No repository interfaces/implementations to maintain
- **Faster Development**: Write queries directly where needed
- **Easier to Understand**: No abstraction - just LINQ queries
- **Easy Testing**: EF Core In-Memory Database works perfectly

### Disadvantages (⚠️)
- **Services Depend on EF Core**: Tight coupling to Entity Framework
- **Not Testable with Mocks**: Must use in-memory database for tests
- **Duplicate Queries**: Same query may appear in multiple services
- **No Abstraction**: Can't easily switch ORMs later

### Trade-offs
- **Simplicity vs. Testability**: POC prioritizes simplicity - in-memory DB is sufficient for tests
- **Speed vs. Architecture**: Fast development > clean architecture for POC
- **POC is Disposable**: This code won't go to production, so coupling is acceptable

## When to Use (POC Context)

### Use Direct DbContext Access When:
- ✅ Building a POC or prototype
- ✅ Code is disposable (won't go to production)
- ✅ Speed is more important than architecture
- ✅ Team is small (1-2 developers)
- ✅ Maximum simplicity is the goal

### Use Repository Pattern Instead When:
- ❌ Building production code (use Final Architecture)
- ❌ Need to mock data access in unit tests
- ❌ Want to centralize complex queries
- ❌ Multiple teams working on codebase

## POC vs Final Architecture

| Aspect | POC (Direct DbContext) | Final Architecture |
|--------|------------------------|--------------------|
| **Pattern** | No abstraction (direct DbContext) | Repository + Specification Pattern |
| **Location** | Queries in service methods | Queries in repository classes |
| **Testability** | In-memory database | Mockable interfaces |
| **Query Reuse** | Copy/paste queries | Centralized in repositories |
| **Coupling** | Tight (services → EF Core) | Loose (services → interfaces) |

## Related Patterns

- **Service Layer Pattern**: Services contain both business logic and data access
- **Active Record Pattern**: Similar - domain objects have data access methods
- **Repository Pattern**: NOT used in POC - added in final architecture

## Examples from CardDemo

### AccountService (Direct DbContext)
**COBOL Files**: ACCTDAT  
**Operations**: All queries directly in service methods
```csharp
var account = await _context.Accounts
    .Include(a => a.Customer)
    .FirstOrDefaultAsync(a => a.AccountNumber == accountNumber);
```

### CardService (Direct DbContext)
**COBOL Files**: CARDDAT  
**Operations**: LINQ queries in service methods
```csharp
var cards = await _context.Cards
    .Where(c => c.AccountId == accountId)
    .ToListAsync();
```

### TransactionService (Direct DbContext)
**COBOL Files**: TRANSACT  
**Operations**: Direct EF Core queries
```csharp
var transactions = await _context.Transactions
    .Where(t => t.AccountId == accountId)
    .OrderByDescending(t => t.TransactionDate)
    .Take(pageSize)
    .ToListAsync();
```

## References

- Microsoft Docs: [Testing with EF Core In-Memory Database](https://learn.microsoft.com/en-us/ef/core/testing/)
- Microsoft Docs: [DbContext Lifetime](https://learn.microsoft.com/en-us/ef/core/dbcontext-configuration/)
- Final Architecture: `../../patterns/PATTERN-001-repository-pattern.md` (will use Repository pattern)

---

**POC Reminder**: Maximum simplicity. DbContext in services is fine for throwaway POC code. Use Repository pattern in final architecture.
