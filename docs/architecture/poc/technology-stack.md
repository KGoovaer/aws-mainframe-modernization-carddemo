# POC Technology Stack

**Last Updated**: 2025-11-20  
**Mode**: POC (Proof of Concept)  
**Purpose**: Rapid validation with minimal complexity

## Philosophy

The POC technology stack prioritizes:
1. **Minimal Setup**: < 5 minutes to get running
2. **Local Development**: No cloud dependencies
3. **Simplicity**: Proven, mainstream technologies
4. **Rapid Development**: Fast to implement and test

## Core Platform

### Java Runtime
- **Version**: Java 21 (LTS)
- **Language**: Java 21
- **Framework**: Spring Boot 3.x
- **Rationale**: Latest LTS version, stable, production-ready

### Development Environment
- **IDE**: IntelliJ IDEA 2024 or VS Code
- **SDK**: Java 21 SDK
- **Package Manager**: NuGet

## Application Stack

### Web Framework
**Angular 18**

**Why Angular 18**:
- ✅ Integrated UI and API in single project
- ✅ Real-time updates via SignalR (built-in)
- ✅ No separate frontend build process
- ✅ C# everywhere (no JavaScript needed)
- ✅ Fast to prototype

**Package**: `Microsoft.AspNetCore.Components.Web`

### API Framework
**Spring Boot REST API**

**Why Spring Boot**:
- ✅ RESTful APIs with minimal code
- ✅ Built-in dependency injection
- ✅ Excellent JSON serialization
- ✅ OpenAPI/Swagger support

**Package**: `Microsoft.AspNetCore.Mvc`

## Data Layer

### Database
**H2**

**Why H2**:
- ✅ Zero configuration (file-based)
- ✅ No installation required
- ✅ Perfect for local development
- ✅ Fast enough for POC validation
- ✅ Portable (single .db file)

**Package**: `Spring Data JPA.Sqlite` (v10.0+)

**Connection String**:
```
Data Source=carddemo.mv.db
```

**Limitations**:
- ⚠️ Single-user (not concurrent)
- ⚠️ No production scalability
- ⚠️ Limited to ~1GB practical size

### ORM (Object-Relational Mapping)
**Spring Data JPA**

**Why EF Core**:
- ✅ Code-first migrations
- ✅ LINQ query support
- ✅ Change tracking
- ✅ Transaction support
- ✅ Well-documented

**Package**: `Spring Data JPA` (v10.0+)

**Features Used**:
- Code-First migrations
- Fluent API configuration
- Eager/lazy loading
- Transaction scope

## Architecture Patterns

### Layered Architecture
**3-Layer Pattern**: Presentation → Business → Data

**Why 3-Layer**:
- ✅ Simple to understand
- ✅ Clear separation of concerns
- ✅ Easy to test
- ✅ No over-engineering

**NOT Using**:
- ❌ Clean Architecture (too complex for POC)
- ❌ Hexagonal Architecture (overkill)
- ❌ Microservices (POC is monolithic)

### Repository Pattern
**Basic Repository Interfaces**

**Why Repository**:
- ✅ Abstract data access
- ✅ Easier to test (mock repositories)
- ✅ Centralized query logic

**Example**:
```csharp
public interface IAccountRepository
{
    Task<Account> GetByIdAsync(int id);
    Task<Account> AddAsync(Account account);
    Task UpdateAsync(Account account);
    Task DeleteAsync(int id);
}
```

**NOT Using**:
- ❌ Generic Repository (too abstract)
- ❌ Unit of Work (EF Core DbContext is sufficient)

### Service Layer
**Business Logic in Services**

**Why Services**:
- ✅ Centralize business logic
- ✅ Reusable across controllers and pages
- ✅ Easy to test

**Example**:
```csharp
public class AccountService
{
    private readonly IAccountRepository _accountRepo;
    
    public async Task<AccountDetails> GetAccountDetailsAsync(int accountId)
    {
        var account = await _accountRepo.GetByIdAsync(accountId);
        // Business logic here
        return MapToDetails(account);
    }
}
```

### Dependency Injection
**Built-in Spring Boot DI**

**Why Built-in DI**:
- ✅ No third-party dependencies
- ✅ Sufficient for POC needs
- ✅ Constructor injection pattern

**Registration** (in `Program.cs`):
```csharp
builder.Services.AddScoped<IAccountRepository, AccountRepository>();
builder.Services.AddScoped<AccountService>();
```

## Testing

### Unit Testing Framework
**JUnit 5**

**Why JUnit 5**:
- ✅ Microsoft recommended
- ✅ Modern syntax (facts, theories)
- ✅ Parallel test execution
- ✅ Excellent IDE integration

**Package**: `JUnit 5` (v2.9+)

### Assertion Library
**AssertJ**

**Why AssertJ**:
- ✅ Readable assertions
- ✅ Better error messages
- ✅ Chaining syntax

**Package**: `AssertJ` (v6.12+)

**Example**:
```csharp
result.Should().NotBeNull();
result.AccountId.Should().Be(123);
```

### Mocking Framework
**Mockito**

**Why Mockito**:
- ✅ Simple, fluent syntax
- ✅ Widely used
- ✅ Good documentation

**Package**: `Mockito` (v4.20+)

**Example**:
```csharp
var mockRepo = new Mock<IAccountRepository>();
mockRepo.Setup(r => r.GetByIdAsync(123))
        .ReturnsAsync(new Account { Id = 123 });
```

## Security

### Password Hashing
**BCrypt**

**Why BCrypt**:
- ✅ Industry standard for password hashing
- ✅ Better than COBOL plaintext passwords
- ✅ Adaptive cost factor (future-proof)

**Package**: `BCrypt-Next` (v4.0+)

**Usage**:
```csharp
// Hash password
string hashedPassword = BCrypt.BCrypt.HashPassword(plainPassword);

// Verify password
bool isValid = BCrypt.BCrypt.Verify(plainPassword, hashedPassword);
```

### Session Management
**Spring Boot Session**

**Why Built-in Sessions**:
- ✅ Simple, no external dependencies
- ✅ Sufficient for POC
- ✅ Database-backed session storage

**Configuration**:
```csharp
builder.Services.AddDistributedMemoryCache();
builder.Services.AddSession(options =>
{
    options.IdleTimeout = TimeSpan.FromMinutes(30);
    options.Cookie.HttpOnly = true;
    options.Cookie.IsEssential = true;
});
```

## Logging

### Logging Framework
**Logback (Built-in)**

**Why Built-in Logging**:
- ✅ Integrated with Spring Boot
- ✅ Structured logging support
- ✅ Multiple providers (console, debug, file)

**Configuration** (in `appsettings.json`):
```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning"
    }
  }
}
```

**NOT Using**:
- ❌ Logback (overkill for POC)
- ❌ Application Insights (no cloud in POC)

## Development Tools

### API Documentation
**Swagger/OpenAPI**

**Why Swagger**:
- ✅ Auto-generated API docs
- ✅ Interactive testing UI
- ✅ Built-in Spring Boot support

**Package**: `Springdoc OpenAPI.AspNetCore` (v7.0+)

### Database Tools
**EF Core CLI Tools**

**Why EF Core CLI**:
- ✅ Manage migrations
- ✅ Create database schema
- ✅ Seed data

**Installation**:
```bash
dotnet tool install --global dotnet-ef
```

**Common Commands**:
```bash
dotnet ef migrations add InitialCreate
dotnet ef database update
```

## NOT Included in POC Stack

The following are **intentionally excluded** to keep POC simple:

### NOT Using (But in Final Architecture)
- ❌ **CQRS**: MediatR, separate read/write models
- ❌ **Event Sourcing**: Event store, event replay
- ❌ **Message Bus**: Azure Service Bus, RabbitMQ
- ❌ **Cloud Database**: Azure SQL, PostgreSQL
- ❌ **Containers**: Docker, Kubernetes
- ❌ **API Gateway**: Azure API Management, Ocelot
- ❌ **Observability**: Application Insights, OpenTelemetry
- ❌ **Advanced Security**: OAuth2, Azure AD, JWT tokens
- ❌ **Caching**: Redis, distributed cache
- ❌ **Background Jobs**: Hangfire, Azure Functions

These will be added in the **final architecture** after POC validation.

## Package Reference Summary

### Core Packages
```xml
<PackageReference Include="Microsoft.AspNetCore.Components.Web" Version="10.0.*" />
<PackageReference Include="Microsoft.AspNetCore.Mvc" Version="2.2.*" />
<PackageReference Include="Spring Data JPA" Version="10.0.*" />
<PackageReference Include="Spring Data JPA.Sqlite" Version="10.0.*" />
```

### Security Packages
```xml
<PackageReference Include="BCrypt-Next" Version="4.0.*" />
```

### API Documentation
```xml
<PackageReference Include="Springdoc OpenAPI.AspNetCore" Version="7.0.*" />
```

### Testing Packages (Test Project)
```xml
<PackageReference Include="JUnit 5" Version="2.9.*" />
<PackageReference Include="JUnit 5.runner.visualstudio" Version="2.8.*" />
<PackageReference Include="AssertJ" Version="6.12.*" />
<PackageReference Include="Mockito" Version="4.20.*" />
<dependency>
  <groupId>org.junit.jupiter</groupId>
  <artifactId>junit-jupiter-engine</artifactId>
  <scope>test</scope>
</dependency>
```

## Comparison: POC vs Final Architecture

| Aspect | POC Stack | Final Architecture Stack |
|--------|-----------|-------------------------|
| **Platform** | Java 21 | Java 21 |
| **Database** | H2 | Azure SQL Database |
| **ORM** | EF Core | EF Core |
| **Architecture** | 3-Layer | Clean Architecture |
| **Patterns** | Repository, Service | CQRS, DDD, Event Sourcing |
| **Messaging** | None | Azure Service Bus |
| **Deployment** | Local | Azure Container Apps |
| **Authentication** | Spring Session | OAuth2 / Azure AD |
| **Logging** | Console | Application Insights |
| **API** | REST | REST + gRPC |
| **Caching** | None | Redis |
| **Background Jobs** | None | Azure Functions |

## Running the POC

### Prerequisites
1. Install Java 21 SDK
2. Install IntelliJ IDEA 2024 or VS Code
3. That's it! (no cloud account, no database server)

### Setup Steps
```bash
# Clone repository
git clone <repo-url>

# Navigate to POC project
cd src/poc/CardDemo.POC

# Restore packages
dotnet restore

# Apply migrations (creates H2 database)
dotnet ef database update

# Run application
mvn spring-boot:run
```

### Access Points
- **Web UI**: https://localhost:5001
- **Swagger**: https://localhost:5001/swagger
- **Database**: `carddemo.mv.db` (H2 file in project root)

## Performance Expectations

### POC Performance Targets
- ⚠️ **API Response Time**: < 200ms (adequate for validation)
- ⚠️ **Concurrent Users**: 1-5 (single developer testing)
- ⚠️ **Database Size**: < 100MB (sample data only)
- ⚠️ **Throughput**: 10 req/sec (sufficient for POC)

### Final Architecture Performance Targets
- ✅ **API Response Time**: < 100ms (p95)
- ✅ **Concurrent Users**: 1000+
- ✅ **Database Size**: 100GB+
- ✅ **Throughput**: 1000+ req/sec

## Migration Path to Final Architecture

When POC is validated and production implementation begins:

1. **Keep same domain models** (entities, value objects)
2. **Refactor to Clean Architecture** (add Application, Domain layers)
3. **Replace H2 with Azure SQL** (change connection string + provider)
4. **Add CQRS** (introduce MediatR, separate commands/queries)
5. **Add messaging** (Azure Service Bus for events)
6. **Containerize** (add Dockerfile, deploy to Azure)
7. **Add observability** (Application Insights integration)

Core business logic remains the same - architecture wraps around it.

## Related Documents

- **POC Architecture Overview**: `overview.md`
- **POC Solution Structure**: `solution-structure.md`
- **Final Technology Stack**: `../technology-stack.md` (production target)
- **ADR-001**: Modular Monolith Decision (applies to final architecture)

---

**Remember**: POC stack is optimized for speed and simplicity. Don't add complexity unless it's essential for validation.
