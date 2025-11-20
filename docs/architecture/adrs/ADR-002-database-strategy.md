# ADR-002: Database Strategy - Single Database with Schema-per-Module

**Status**: Accepted  
**Date**: 2025-11-20  
**Deciders**: Software Architect  
**Consulted**: Development Team, DBA

## Context

The legacy CardDemo application uses VSAM (Virtual Storage Access Method) files for data storage on the mainframe. As we modernize to .NET and Azure, we need to decide:

1. **Database Technology**: SQL vs. NoSQL vs. Hybrid
2. **Database Organization**: Single database vs. multiple databases vs. database-per-module
3. **Schema Organization**: How to organize tables within the database
4. **Data Isolation**: How to maintain module boundaries at the data layer
5. **Transaction Management**: How to handle cross-module transactions
6. **Migration Path**: How to migrate from VSAM to relational database

### Legacy VSAM Structure

The current mainframe system uses separate VSAM files:
- `ACCTFILE` - Account data (ACCTDAT.txt)
- `CARDFILE` - Card data (CARDDAT.txt)
- `CUSTFILE` - Customer data (CUSTDAT.txt)
- `TRANFILE` - Transaction data (TRANDAT.txt)
- `CARDXRF` - Card cross-reference (CARDXRF.txt)
- `CUSTXRF` - Customer cross-reference (CXACAIX.txt)
- `TRXCAT` - Transaction category (TRANCATG.txt)
- `DISCGRP` - Disclosure group (DISCGRP.txt)

Each file is accessed independently, with explicit file I/O in COBOL programs.

### Key Considerations

1. **Module Boundaries**: We've defined 7 bounded contexts (modules) that should maintain clear separation
2. **Transactions**: Many business operations span multiple modules (e.g., posting transaction affects account, card, and transaction data)
3. **Data Consistency**: Need ACID transactions for financial operations
4. **Scalability**: Read-heavy operations (queries, reports) vs. write-heavy operations (transaction posting)
5. **Cost**: Azure SQL Database pricing based on DTUs/vCores
6. **Operational Complexity**: Managing multiple databases vs. single database
7. **Migration Complexity**: Migrating from VSAM to relational database

### Options Considered

#### Option 1: Single Azure SQL Database with Schema-per-Module

**Description**: One database with separate schemas for each module.

**Structure**:
```
Database: carddemo-prod
├── Schema: account
│   ├── Accounts
│   ├── AccountCards
│   └── AccountGroups
├── Schema: card
│   ├── Cards
│   ├── CardTypes
│   └── CardActivations
├── Schema: customer
│   ├── Customers
│   └── CustomerAddresses
├── Schema: transaction
│   ├── Transactions
│   ├── TransactionCategories
│   └── TransactionTypes
├── Schema: user
│   ├── Users
│   ├── UserRoles
│   └── UserPermissions
├── Schema: batch
│   └── BatchJobs
└── Schema: reference
    ├── DisclosureGroups
    └── InterestRates
```

**EF Core Implementation**:
```csharp
// Infrastructure.Persistence/AccountDbContext.cs
public class AccountDbContext : DbContext
{
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        // All entities in 'account' schema
        modelBuilder.HasDefaultSchema("account");
        
        modelBuilder.ApplyConfigurationsFromAssembly(
            typeof(AccountDbContext).Assembly);
    }
}

// Infrastructure.Persistence/Configurations/AccountConfiguration.cs
public class AccountConfiguration : IEntityTypeConfiguration<Account>
{
    public void Configure(EntityTypeBuilder<Account> builder)
    {
        builder.ToTable("Accounts", "account");
        builder.HasKey(a => a.Id);
        // ... configuration
    }
}
```

**Migration Pattern**:
```csharp
// Migration example
public partial class CreateAccountSchema : Migration
{
    protected override void Up(MigrationBuilder migrationBuilder)
    {
        migrationBuilder.EnsureSchema("account");
        
        migrationBuilder.CreateTable(
            name: "Accounts",
            schema: "account",
            columns: table => new
            {
                Id = table.Column<Guid>(nullable: false),
                AccountNumber = table.Column<string>(maxLength: 11, nullable: false),
                CustomerId = table.Column<string>(maxLength: 11, nullable: false),
                CreditLimit = table.Column<decimal>(precision: 18, scale: 2, nullable: false),
                // ... more columns
            });
    }
}
```

**Pros**:
- ✅ **Clear Module Boundaries**: Schemas provide logical separation
- ✅ **ACID Transactions**: Easy to span multiple schemas in single transaction
- ✅ **Cost Effective**: One database to pay for and manage
- ✅ **Operational Simplicity**: Single connection string, backup, monitoring
- ✅ **Cross-Module Queries**: Can join across schemas when needed (reports)
- ✅ **Schema-Based Security**: Can grant permissions per schema
- ✅ **Easier Migration**: Single target database for VSAM data

**Cons**:
- ⚠️ **Coupling Risk**: Developers could create cross-schema foreign keys
- ⚠️ **Scaling**: Can't scale individual schemas independently
- ⚠️ **Resource Contention**: All modules share database resources

#### Option 2: Database-per-Module (Polyglot Persistence)

**Description**: Separate database for each module, potentially different database types.

**Structure**:
```
carddemo-account-db (Azure SQL)
carddemo-card-db (Azure SQL)
carddemo-customer-db (Azure SQL)
carddemo-transaction-db (Azure SQL)
carddemo-user-db (Azure SQL)
carddemo-analytics-db (Cosmos DB) - for reports
```

**Pros**:
- ✅ **True Module Isolation**: Physical separation prevents coupling
- ✅ **Independent Scaling**: Scale each database independently
- ✅ **Technology Flexibility**: Use NoSQL for specific modules if beneficial
- ✅ **Blast Radius**: Issue in one database doesn't affect others
- ✅ **Microservices Ready**: Already prepared if we split to microservices

**Cons**:
- ❌ **No Distributed Transactions**: Cross-database transactions require saga pattern
- ❌ **Data Consistency**: Eventual consistency across modules
- ❌ **Cost**: 5-7 separate databases to pay for
- ❌ **Operational Complexity**: Multiple connection strings, backups, monitoring
- ❌ **Cross-Module Queries**: Reports spanning modules become very complex
- ❌ **Migration Complexity**: Need to carefully partition VSAM data

#### Option 3: Hybrid - Single Database with Multiple DbContexts

**Description**: One database, but each module has its own DbContext that only maps its own tables.

**Structure**:
```
Database: carddemo-prod
Tables (no schemas):
├── Accounts
├── Cards
├── Customers
├── Transactions
└── ...

DbContexts:
├── AccountDbContext -> Accounts, AccountCards
├── CardDbContext -> Cards, CardTypes
└── TransactionDbContext -> Transactions
```

**Pros**:
- ✅ **Logical Separation**: DbContext boundaries enforce module separation
- ✅ **Simple Database**: No schema complexity
- ✅ **ACID Transactions**: Can still use distributed transactions if needed

**Cons**:
- ⚠️ **No Physical Separation**: All tables in default 'dbo' schema
- ⚠️ **Naming Conflicts**: Need careful table naming conventions
- ⚠️ **Less Clear Boundaries**: Developers might query wrong tables directly

#### Option 4: Microservices Database Pattern (Future State)

**Description**: Each microservice owns its database, API-based integration only.

**Pros**:
- ✅ **True Independence**: Complete service autonomy
- ✅ **Polyglot Persistence**: Best database for each service

**Cons**:
- ❌ **Premature**: We're starting with modular monolith, not microservices
- ❌ **Complexity**: Too complex for initial implementation
- ❌ **Team Readiness**: Team is transitioning from mainframe

## Decision

**We will use Option 1: Single Azure SQL Database with Schema-per-Module**

### Implementation Details

#### Database Configuration

**Database**: `carddemo-prod` (Azure SQL Database)  
**Pricing Tier**: Standard S3 (100 DTUs) initially, scale as needed  
**Backup**: Automated backups with 7-day retention  
**Geo-Replication**: Enabled for disaster recovery  

#### Schema Organization

```sql
-- Module schemas
CREATE SCHEMA [account] AUTHORIZATION dbo;
CREATE SCHEMA [card] AUTHORIZATION dbo;
CREATE SCHEMA [customer] AUTHORIZATION dbo;
CREATE SCHEMA [transaction] AUTHORIZATION dbo;
CREATE SCHEMA [user] AUTHORIZATION dbo;
CREATE SCHEMA [batch] AUTHORIZATION dbo;
CREATE SCHEMA [reference] AUTHORIZATION dbo;
```

#### EF Core DbContext per Module

Each module has its own `DbContext`:

```csharp
// CardDemo.Infrastructure/Persistence/AccountDbContext.cs
public class AccountDbContext : DbContext
{
    public AccountDbContext(DbContextOptions<AccountDbContext> options) 
        : base(options) { }
    
    public DbSet<Account> Accounts { get; set; }
    public DbSet<AccountGroup> AccountGroups { get; set; }
    
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.HasDefaultSchema("account");
        modelBuilder.ApplyConfigurationsFromAssembly(typeof(AccountDbContext).Assembly);
    }
}

// CardDemo.Infrastructure/Persistence/CardDbContext.cs
public class CardDbContext : DbContext
{
    public CardDbContext(DbContextOptions<CardDbContext> options) 
        : base(options) { }
    
    public DbSet<Card> Cards { get; set; }
    public DbSet<CardType> CardTypes { get; set; }
    
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.HasDefaultSchema("card");
        modelBuilder.ApplyConfigurationsFromAssembly(typeof(CardDbContext).Assembly);
    }
}
```

#### Connection String Management

**appsettings.json** (single connection string):
```json
{
  "ConnectionStrings": {
    "CardDemoDatabase": "Server=tcp:carddemo-sql.database.windows.net,1433;Database=carddemo-prod;..."
  }
}
```

**Dependency Injection** (multiple DbContexts):
```csharp
services.AddDbContext<AccountDbContext>(options =>
    options.UseSqlServer(configuration.GetConnectionString("CardDemoDatabase")));

services.AddDbContext<CardDbContext>(options =>
    options.UseSqlServer(configuration.GetConnectionString("CardDemoDatabase")));

services.AddDbContext<TransactionDbContext>(options =>
    options.UseSqlServer(configuration.GetConnectionString("CardDemoDatabase")));
```

#### Cross-Module References

**Rule**: No foreign keys across schemas, use reference by ID only.

**Example** (Account references Customer):
```csharp
// ❌ BAD: Foreign key to customer schema
public class Account
{
    public Guid Id { get; set; }
    public Customer Customer { get; set; } // Don't navigate to customer schema
}

// ✅ GOOD: Reference by ID only
public class Account
{
    public Guid Id { get; set; }
    public string CustomerId { get; set; } // Store ID, query via API/MediatR
}
```

To get customer data from account module:
```csharp
// Use MediatR to query across modules (in-process)
var customer = await _mediator.Send(new GetCustomerQuery(account.CustomerId));
```

#### Transaction Management

**Single-Module Transactions** (most common):
```csharp
// Uses DbContext.SaveChangesAsync() - automatic transaction
public class CreateAccountCommandHandler : IRequestHandler<CreateAccountCommand, Result<Guid>>
{
    private readonly AccountDbContext _context;
    
    public async Task<Result<Guid>> Handle(CreateAccountCommand request, CancellationToken ct)
    {
        var account = Account.Create(...);
        _context.Accounts.Add(account);
        await _context.SaveChangesAsync(ct); // Atomic transaction
        return Result<Guid>.Success(account.Id);
    }
}
```

**Cross-Module Transactions** (when needed):
```csharp
// Use TransactionScope for distributed transaction across DbContexts
public class PostTransactionCommandHandler : IRequestHandler<PostTransactionCommand, Result>
{
    private readonly AccountDbContext _accountContext;
    private readonly TransactionDbContext _transactionContext;
    
    public async Task<Result> Handle(PostTransactionCommand request, CancellationToken ct)
    {
        using var scope = new TransactionScope(TransactionScopeAsyncFlowOption.Enabled);
        
        // Update account balance
        var account = await _accountContext.Accounts.FindAsync(request.AccountId);
        account.DebitBalance(request.Amount);
        await _accountContext.SaveChangesAsync(ct);
        
        // Create transaction record
        var transaction = Transaction.Create(request.AccountId, request.Amount, ...);
        _transactionContext.Transactions.Add(transaction);
        await _transactionContext.SaveChangesAsync(ct);
        
        scope.Complete(); // Commit both
        return Result.Success();
    }
}
```

#### Migration from VSAM

**ETL Process** (Azure Data Factory):
```
VSAM Files (Mainframe) 
  → Extract to CSV → Azure Blob Storage 
  → Transform (Data Factory) → Azure SQL Database

Mapping:
- ACCTFILE → account.Accounts
- CARDFILE → card.Cards
- CUSTFILE → customer.Customers
- TRANFILE → transaction.Transactions
- CARDXRF → card.CardReferences
- CUSTXRF → customer.CustomerReferences
- TRXCAT → reference.TransactionCategories
- DISCGRP → reference.DisclosureGroups
```

**Validation**:
- Row count validation per file
- Checksum validation for key fields
- Sample data comparison (first 1000 records)

#### Security

**Schema-Level Permissions**:
```sql
-- Application user has access to all schemas
CREATE USER [carddemo-app-user] WITH PASSWORD = '...';
GRANT SELECT, INSERT, UPDATE, DELETE ON SCHEMA::account TO [carddemo-app-user];
GRANT SELECT, INSERT, UPDATE, DELETE ON SCHEMA::card TO [carddemo-app-user];
-- ... etc for all schemas

-- Read-only user for reporting
CREATE USER [carddemo-report-user] WITH PASSWORD = '...';
GRANT SELECT ON SCHEMA::account TO [carddemo-report-user];
GRANT SELECT ON SCHEMA::transaction TO [carddemo-report-user];
-- ... etc
```

#### Monitoring

- **Query Performance**: Track slow queries per schema
- **Resource Usage**: Monitor DTU usage, identify bottleneck schemas
- **Connection Pooling**: Monitor connection pool per DbContext
- **Deadlocks**: Alert on deadlock events

## Rationale

### Why Single Database?

1. **ACID Transactions**: Financial application requires strong consistency
   - Account balance updates must be atomic with transaction records
   - Cross-module operations are common (e.g., card activation updates account and card)
   - No need for eventual consistency complexity (saga pattern)

2. **Cost Optimization**: 
   - One database to manage and pay for
   - CardDemo is not Netflix-scale, single database can handle load
   - Standard S3 tier (~$150/month) vs. 7 databases (~$1000/month)

3. **Operational Simplicity**:
   - Single connection string
   - One backup strategy
   - One monitoring dashboard
   - Easier disaster recovery

4. **Cross-Module Queries**:
   - Reports often span modules (e.g., customer account statement with transactions)
   - Can JOIN across schemas when needed
   - No need for complex ETL to reporting database

5. **Migration Simplicity**:
   - Single target database for VSAM migration
   - Can validate entire dataset in one place
   - Easier rollback if issues occur

### Why Schema-per-Module?

1. **Logical Separation**:
   - Clear boundaries between modules
   - Visual separation in database tools (schema prefix)
   - Easy to identify which tables belong to which module

2. **Security**:
   - Can grant schema-level permissions
   - Future-proof if we need to restrict access per module

3. **Organization**:
   - Prevents table naming conflicts
   - Clear ownership of database objects
   - Easier to navigate in large database

4. **Migration Path**:
   - If we later split to microservices, schemas can become separate databases
   - Minimal code changes (just connection strings)
   - DbContext already separated per module

### Why NOT Database-per-Module?

While database-per-module provides better isolation, it's premature for CardDemo:

1. **Team Maturity**: Team is transitioning from mainframe, start simpler
2. **Complexity**: Eventual consistency and saga patterns add significant complexity
3. **ACID Requirements**: Financial transactions need strong consistency
4. **Cost**: Unjustified for current scale
5. **YAGNI Principle**: We don't need microservices-level isolation yet

**Decision**: Start simple (single database), refactor later if needed.

## Consequences

### Positive

- ✅ **Strong Consistency**: ACID transactions across all modules
- ✅ **Cost Effective**: ~85% cost reduction vs. multiple databases
- ✅ **Operational Simplicity**: Single database to manage
- ✅ **Cross-Module Queries**: Easy reporting and analytics
- ✅ **Clear Module Boundaries**: Schemas provide logical separation
- ✅ **Migration Path**: Can split to multiple databases later if needed
- ✅ **Schema-Level Security**: Can restrict access per module
- ✅ **Easier VSAM Migration**: Single target database

### Negative

- ⚠️ **Coupling Risk**: Developers could create cross-schema foreign keys (mitigated by code reviews)
- ⚠️ **Scaling Limitations**: Can't scale individual schemas independently (acceptable for CardDemo scale)
- ⚠️ **Resource Contention**: All modules share database resources (monitor and scale vertically if needed)
- ⚠️ **Single Point of Failure**: Database outage affects all modules (mitigated by Azure SLA and geo-replication)

### Mitigations

**Prevent Cross-Schema Coupling**:
- ✅ Architecture tests (ArchUnit.NET) to prevent cross-schema foreign keys
- ✅ Code reviews to enforce reference-by-ID pattern
- ✅ MediatR for cross-module queries (in-process, but explicit)
- ✅ Documentation and developer guidelines

**Scaling Strategy**:
- ✅ Start with Standard S3 (100 DTUs)
- ✅ Monitor performance, scale vertically if needed
- ✅ Use read replicas for reporting workloads
- ✅ Implement Redis caching for frequently accessed data
- ✅ If we hit limits, can split to multiple databases later

**Resilience**:
- ✅ Azure SQL Database 99.99% SLA
- ✅ Geo-replication for disaster recovery
- ✅ Automated backups with point-in-time restore
- ✅ Connection pooling and retry policies

## Validation

We will validate this decision by:

1. **Performance Testing**: Load test with expected transaction volume
   - Target: < 50ms query response time (p95)
   - Target: 1000 TPS (transactions per second)

2. **Cost Analysis**: Track monthly database costs vs. budget
   - Monitor DTU usage
   - Identify optimization opportunities

3. **Developer Feedback**: Survey team after 3 months
   - Is schema separation working?
   - Any coupling issues?
   - Any performance issues?

4. **Architectural Review**: Review after 6 months
   - Have we stayed within single-database capabilities?
   - Is it time to consider database-per-module?

### Success Criteria

- ✅ All database operations < 50ms (p95)
- ✅ No cross-schema foreign keys in codebase
- ✅ Monthly database cost < $300
- ✅ 99.9%+ uptime
- ✅ Developer satisfaction with schema organization

### Exit Criteria (When to Split to Multiple Databases)

If we encounter any of these, reconsider database-per-module:

- Database DTU usage consistently > 80%
- Cross-module transactions causing significant deadlocks
- Module scaling requirements diverge significantly
- Decision to split into microservices
- Specific module needs NoSQL (e.g., document storage)

## Related Decisions

- **ADR-001**: Use Modular Monolith over Microservices (aligns with single database)
- **ADR-003**: CQRS with MediatR (enables cross-module queries without tight coupling)
- **ADR-007**: Entity Framework Core for Data Access (implements schema-per-module pattern)

## References

- [Azure SQL Database Documentation](https://learn.microsoft.com/en-us/azure/azure-sql/database/)
- [EF Core Schema Configuration](https://learn.microsoft.com/en-us/ef/core/modeling/relational/schemas)
- [Database per Service Pattern - Microservices.io](https://microservices.io/patterns/data/database-per-service.html)
- [Shared Database Anti-Pattern](https://www.enterpriseintegrationpatterns.com/patterns/conversation/SharedDatabase.html)
- [SQL Server Schemas](https://learn.microsoft.com/en-us/sql/relational-databases/security/authentication-access/create-a-database-schema)
