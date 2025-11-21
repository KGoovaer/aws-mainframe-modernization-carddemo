# POC Solution Structure

**Last Updated**: 2025-11-20  
**Mode**: POC (Proof of Concept)  
**Purpose**: Simplified structure for rapid validation

## Overview

The POC uses a **simplified, single-project structure** optimized for rapid development and validation. This is intentionally simpler than the final production architecture.

## Philosophy

### POC Structure Principles
1. **Single Project**: All code in one project for simplicity
2. **Folder-Based Organization**: Use folders, not projects, for separation
3. **Minimal Configuration**: Fewer files, less complexity
4. **Fast to Navigate**: Easy for developers to find code
5. **Easy to Refactor**: Can extract to proper layers later

### NOT Following (Saved for Final Architecture)
- ❌ Multi-project solution (Domain, Application, Infrastructure)
- ❌ Clean Architecture layers (too complex for POC)
- ❌ Separate test projects per layer
- ❌ Microservices (POC is monolithic)

## Solution Structure

```
CardDemo.POC/                                    # Solution root
│
├── CardDemo.POC.sln                            # Visual Studio solution file
│
├── carddemo-poc/                           # Main web application project
│   ├── Controllers/                            # API controllers
│   │   ├── AuthController.java                  # Authentication endpoints
│   │   ├── AccountsController.java              # Account management API
│   │   ├── CardsController.java                 # Card management API
│   │   └── TransactionsController.java          # Transaction API
│   │
│   ├── Pages/                                  # Angular pages (UI)
│   │   ├── Index.razor                        # Home/dashboard
│   │   ├── Login.razor                        # Login page (COSGN00)
│   │   ├── Menu.razor                         # Main menu (COMEN01)
│   │   ├── Accounts/
│   │   │   ├── AccountDetails.razor           # View account (COACTVW)
│   │   │   └── AccountEdit.razor              # Edit account (COACTUP)
│   │   ├── Cards/
│   │   │   ├── CardList.razor                 # Card list (COCRDLI)
│   │   │   ├── CardDetails.razor              # Card details (COCRDSL)
│   │   │   └── CardEdit.razor                 # Edit card (COCRDUP)
│   │   ├── Transactions/
│   │   │   ├── TransactionList.razor          # Transaction list (COTRN00)
│   │   │   ├── TransactionDetails.razor       # Transaction details (COTRN01)
│   │   │   └── TransactionAdd.razor           # Add transaction (COTRN02)
│   │   └── Admin/
│   │       ├── UserList.razor                 # User management (COUSR00)
│   │       ├── Reports.razor                  # Report generation (CORPT00)
│   │       └── Billing.razor                  # Bill payment (COBIL00)
│   │
│   ├── Services/                               # Business logic services
│   │   ├── AuthenticationService.java           # Login, logout, session mgmt
│   │   ├── AccountService.java                  # Account CRUD + interest calc
│   │   ├── CardService.java                     # Card CRUD operations
│   │   ├── TransactionService.java              # Transaction processing
│   │   ├── UserService.java                     # User management
│   │   └── ReportService.java                   # Report generation
│   │
│   ├── Data/                                   # Data access layer
│   │   ├── CardDemoDbContext.java               # EF Core DbContext
│   │   ├── Entities/                          # Database entity models
│   │   │   ├── User.java                        # User entity (USRSEC)
│   │   │   ├── Account.java                     # Account entity (ACCTDAT)
│   │   │   ├── Customer.java                    # Customer entity (CUSTDAT)
│   │   │   ├── Card.java                        # Card entity (CARDDAT)
│   │   │   ├── Transaction.java                 # Transaction entity (TRANSACT)
│   │   │   ├── CategoryBalance.java             # Category balance (TRANCATG)
│   │   │   └── UserSession.java                 # Session tracking
│   │   ├── Repositories/                      # Repository implementations
│   │   │   ├── IAccountRepository.java          # Account repository interface
│   │   │   ├── AccountRepository.java           # Account repository impl
│   │   │   ├── ICardRepository.java             # Card repository interface
│   │   │   ├── CardRepository.java              # Card repository impl
│   │   │   ├── ITransactionRepository.java      # Transaction repository interface
│   │   │   ├── TransactionRepository.java       # Transaction repository impl
│   │   │   ├── IUserRepository.java             # User repository interface
│   │   │   └── UserRepository.java              # User repository impl
│   │   └── Migrations/                        # EF Core migrations
│   │       └── *.java                           # Auto-generated migration files
│   │
│   ├── Models/                                 # DTOs and view models
│   │   ├── Requests/                          # API request models
│   │   │   ├── LoginRequest.java
│   │   │   ├── UpdateAccountRequest.java
│   │   │   ├── UpdateCardRequest.java
│   │   │   └── AddTransactionRequest.java
│   │   ├── Responses/                         # API response models
│   │   │   ├── LoginResponse.java
│   │   │   ├── AccountDetailsResponse.java
│   │   │   ├── CardDetailsResponse.java
│   │   │   └── TransactionDetailsResponse.java
│   │   └── ViewModels/                        # Angular view models
│   │       ├── AccountViewModel.java
│   │       ├── CardViewModel.java
│   │       └── TransactionViewModel.java
│   │
│   ├── Shared/                                 # Shared Angular components
│   │   ├── MainLayout.razor                   # Main layout template
│   │   ├── NavMenu.razor                      # Navigation menu
│   │   └── ErrorBoundary.razor                # Error handling
│   │
│   ├── wwwroot/                                # Static files
│   │   ├── css/
│   │   │   └── app.javas                        # Application styles
│   │   ├── js/
│   │   │   └── app.js                         # JavaScript (if needed)
│   │   └── favicon.ico
│   │
│   ├── Program.java                              # Application entry point
│   ├── appsettings.json                        # Configuration
│   ├── appsettings.Development.json            # Dev configuration
│   └── carddemo-pocpom.xml                # Project file
│
└── CardDemo.POC.Tests/                         # Test project
    ├── Services/                               # Service tests
    │   ├── AuthenticationServiceTests.java
    │   ├── AccountServiceTests.java
    │   ├── CardServiceTests.java
    │   └── TransactionServiceTests.java
    ├── Repositories/                           # Repository tests
    │   ├── AccountRepositoryTests.java
    │   ├── CardRepositoryTests.java
    │   └── TransactionRepositoryTests.java
    ├── Helpers/                                # Test helpers
    │   ├── TestDbContextFactory.java            # In-memory database for tests
    │   └── TestDataBuilder.java                 # Test data generation
    └── CardDemo.POC.Testspom.xml              # Test project file
```

## Folder Responsibilities

### `/Controllers` - API Endpoints
**Purpose**: Handle HTTP requests, return JSON responses  
**Pattern**: Thin controllers, delegate to services  
**Example**:
```csharp
[ApiController]
[Route("api/[controller]")]
public class AccountsController : ControllerBase
{
    private readonly AccountService _accountService;
    
    [HttpGet("{id}")]
    public async Task<IActionResult> GetAccount(int id)
    {
        var account = await _accountService.GetAccountDetailsAsync(id);
        return Ok(account);
    }
}
```

### `/Pages` - Angular UI
**Purpose**: User-facing web pages  
**Pattern**: Code-behind pattern, data binding  
**COBOL Mapping**: Each page maps to COBOL screen/program  
**Example**:
```razor
@page "/accounts/{AccountId:int}"
@inject AccountService AccountService

<h1>Account Details</h1>
<div>Account #: @account.AccountNumber</div>
<div>Balance: @account.CurrentBalance</div>

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

### `/Services` - Business Logic
**Purpose**: Implement business rules and orchestration  
**Pattern**: Service layer pattern  
**Responsibilities**:
- Validate input
- Enforce business rules
- Coordinate multiple repositories
- Transaction management
- Map entities to DTOs

**Example**:
```csharp
public class AccountService
{
    private readonly IAccountRepository _accountRepo;
    private readonly ICustomerRepository _customerRepo;
    
    public async Task<AccountDetailsResponse> GetAccountDetailsAsync(int accountId)
    {
        // Business logic: retrieve account + customer
        var account = await _accountRepo.GetByIdAsync(accountId);
        var customer = await _customerRepo.GetByIdAsync(account.CustomerId);
        
        // Map to DTO
        return new AccountDetailsResponse
        {
            AccountNumber = account.AccountNumber,
            CustomerName = $"{customer.FirstName} {customer.LastName}",
            CurrentBalance = account.CurrentBalance,
            // ... more mappings
        };
    }
}
```

### `/Data/Entities` - Database Models
**Purpose**: EF Core entity classes (map to database tables)  
**Pattern**: Anemic domain model (data only, no behavior)  
**COBOL Mapping**: Maps to VSAM file copybooks  
**Example**:
```csharp
public class Account
{
    public int Id { get; set; }
    public long AccountNumber { get; set; }
    public int CustomerId { get; set; }
    public decimal CurrentBalance { get; set; }
    public decimal CreditLimit { get; set; }
    public string AccountStatus { get; set; }
    
    // Navigation properties
    public Customer Customer { get; set; }
    public List<Card> Cards { get; set; }
    public List<Transaction> Transactions { get; set; }
}
```

### `/Data/Repositories` - Data Access
**Purpose**: Encapsulate database queries  
**Pattern**: Repository pattern  
**Example**:
```csharp
public interface IAccountRepository
{
    Task<Account> GetByIdAsync(int id);
    Task<Account> GetByAccountNumberAsync(long accountNumber);
    Task<List<Account>> GetByCustomerIdAsync(int customerId);
    Task<Account> AddAsync(Account account);
    Task UpdateAsync(Account account);
    Task DeleteAsync(int id);
}

public class AccountRepository : IAccountRepository
{
    private readonly CardDemoDbContext _context;
    
    public async Task<Account> GetByIdAsync(int id)
    {
        return await _context.Accounts
            .Include(a => a.Customer)
            .Include(a => a.Cards)
            .FirstOrDefaultAsync(a => a.Id == id);
    }
    
    // ... other methods
}
```

### `/Models/Requests` - API Input Models
**Purpose**: Validate and shape incoming API data  
**Pattern**: Data annotations for validation  
**Example**:
```csharp
public class UpdateAccountRequest
{
    [Required]
    public long AccountNumber { get; set; }
    
    [Range(0, 999999999)]
    public decimal CreditLimit { get; set; }
    
    [MaxLength(1)]
    public string AccountStatus { get; set; }
}
```

### `/Models/Responses` - API Output Models
**Purpose**: Shape outgoing API data  
**Pattern**: DTOs (Data Transfer Objects)  
**Example**:
```csharp
public class AccountDetailsResponse
{
    public long AccountNumber { get; set; }
    public string CustomerName { get; set; }
    public decimal CurrentBalance { get; set; }
    public decimal CreditLimit { get; set; }
    public string AccountStatus { get; set; }
    public List<CardSummary> Cards { get; set; }
}
```

## Database Context Configuration

### `CardDemoDbContext.java`
```csharp
public class CardDemoDbContext : DbContext
{
    public CardDemoDbContext(DbContextOptions<CardDemoDbContext> options)
        : base(options) { }
    
    public DbSet<User> Users { get; set; }
    public DbSet<Account> Accounts { get; set; }
    public DbSet<Customer> Customers { get; set; }
    public DbSet<Card> Cards { get; set; }
    public DbSet<Transaction> Transactions { get; set; }
    public DbSet<CategoryBalance> CategoryBalances { get; set; }
    
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        // Configure relationships
        modelBuilder.Entity<Account>()
            .HasOne(a => a.Customer)
            .WithMany(c => c.Accounts)
            .HasForeignKey(a => a.CustomerId);
        
        modelBuilder.Entity<Card>()
            .HasOne(c => c.Account)
            .WithMany(a => a.Cards)
            .HasForeignKey(c => c.AccountId);
        
        // Configure indexes
        modelBuilder.Entity<Account>()
            .HasIndex(a => a.AccountNumber)
            .IsUnique();
        
        // More configuration...
    }
}
```

## Application Startup (`Program.java`)

```csharp
var builder = WebApplication.CreateBuilder(args);

// Add services
builder.Services.AddRazorPages();
builder.Services.AddServerSideAngular();
builder.Services.AddControllers();

// Database
builder.Services.AddDbContext<CardDemoDbContext>(options =>
    options.UseSqlite("Data Source=carddemo.db"));

// Repositories
builder.Services.AddScoped<IAccountRepository, AccountRepository>();
builder.Services.AddScoped<ICardRepository, CardRepository>();
builder.Services.AddScoped<ITransactionRepository, TransactionRepository>();
builder.Services.AddScoped<IUserRepository, UserRepository>();

// Services
builder.Services.AddScoped<AuthenticationService>();
builder.Services.AddScoped<AccountService>();
builder.Services.AddScoped<CardService>();
builder.Services.AddScoped<TransactionService>();
builder.Services.AddScoped<UserService>();

// Swagger
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

// Configure middleware
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();
app.UseStaticFiles();
app.UseRouting();

app.MapAngularHub();
app.MapFallbackToPage("/_Host");
app.MapControllers();

app.Run();
```

## Configuration (`appsettings.json`)

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning"
    }
  },
  "AllowedHosts": "*",
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=carddemo.db"
  },
  "CardDemo": {
    "MaxLoginAttempts": 3,
    "SessionTimeoutMinutes": 30,
    "InterestRateAnnual": 0.12,
    "PageSize": 10
  }
}
```

## Comparison: POC vs Final Architecture

| Aspect | POC Structure | Final Architecture Structure |
|--------|---------------|------------------------------|
| **Projects** | 2 (Web + Tests) | 8+ (Domain, Application, Infrastructure, API, etc.) |
| **Layers** | 3 (Presentation, Business, Data) | 4 (Presentation, Application, Domain, Infrastructure) |
| **Patterns** | Repository, Service | CQRS, DDD, Repository, Unit of Work |
| **Folders** | Simple, flat | Deep, organized by feature/layer |
| **Complexity** | Low | High |
| **Setup Time** | < 5 minutes | 30+ minutes |

## Testing Structure

### Unit Tests
**Location**: `CardDemo.POC.Tests/Services/`  
**Pattern**: Arrange-Act-Assert (AAA)  
**Mocking**: Use Moq for repository mocks

**Example**:
```csharp
public class AccountServiceTests
{
    [Fact]
    public async Task GetAccountDetailsAsync_WithValidId_ReturnsAccount()
    {
        // Arrange
        var mockRepo = new Mock<IAccountRepository>();
        mockRepo.Setup(r => r.GetByIdAsync(123))
                .ReturnsAsync(new Account { Id = 123, AccountNumber = 1000000001 });
        
        var service = new AccountService(mockRepo.Object);
        
        // Act
        var result = await service.GetAccountDetailsAsync(123);
        
        // Assert
        result.Should().NotBeNull();
        result.AccountNumber.Should().Be(1000000001);
    }
}
```

### Test Database
**Pattern**: In-memory H2 for integration tests

```csharp
public class TestDbContextFactory
{
    public static CardDemoDbContext CreateInMemoryDatabase()
    {
        var options = new DbContextOptionsBuilder<CardDemoDbContext>()
            .UseSqlite("DataSource=:memory:")
            .Options;
        
        var context = new CardDemoDbContext(options);
        context.Database.OpenConnection();
        context.Database.EnsureCreated();
        
        return context;
    }
}
```

## Migration to Final Architecture

When POC is validated, refactor to final structure:

1. **Extract Domain Layer**: Move entities to separate `Domain` project
2. **Extract Application Layer**: Move services to `Application` project with CQRS
3. **Separate Infrastructure**: Move repositories to `Infrastructure` project
4. **Add API Project**: Separate API from Angular UI
5. **Multi-Project Solution**: Proper Clean Architecture layers
6. **Add Domain Events**: Implement event-driven patterns
7. **Replace H2**: Migrate to Azure SQL Database

Core business logic (services) remains largely the same.

## Related Documents

- **POC Architecture Overview**: `overview.md`
- **POC Technology Stack**: `technology-stack.md`
- **Final Solution Structure**: `../solution-structure.md` (production target)
- **COBOL Source Mapping**: `../../analysis/cobol/` (what we're modernizing)

---

**Remember**: POC structure prioritizes speed over perfection. Keep it simple, validate the concept, then properly architect for production.
