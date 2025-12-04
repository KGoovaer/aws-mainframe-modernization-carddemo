# Solution Structure

**Document Version**: 1.0  
**Last Updated**: 2025-11-20  
**Status**: Draft  
**Owner**: Software Architect

## Overview

This document defines the complete folder structure, project organization, and file naming conventions for the modernized CardDemo Spring Boot solution. The structure follows Clean Architecture principles with clear separation of concerns across Domain, Application, Infrastructure, and Presentation layers.

## Solution Organization

### Root Directory Structure

```
CardDemo.Modernized/
├── .github/                          # GitHub configuration
│   ├── workflows/                    # CI/CD pipelines (GitHub Actions)
│   │   ├── build-and-test.yml
│   │   ├── deploy-dev.yml
│   │   └── deploy-prod.yml
│   └── CODEOWNERS                    # Code review assignments
├── docs/                             # Documentation (existing structure)
│   ├── architecture/                 # Architecture documentation
│   ├── analysis/                     # Business requirements, use cases
│   ├── implementation/               # Component documentation
│   └── testing/                      # Test plans and reports
├── src/                              # Source code
│   ├── Core/                         # Core business logic layers
│   │   ├── CardDemo.Domain/
│   │   └── CardDemo.Application/
│   ├── Infrastructure/               # Infrastructure concerns
│   │   ├── CardDemo.Infrastructure/
│   │   └── CardDemo.Infrastructure.Messaging/
│   ├── Modules/                      # Feature modules (bounded contexts)
│   │   ├── CardDemo.Modules.Authentication/
│   │   ├── CardDemo.Modules.Accounts/
│   │   ├── CardDemo.Modules.Cards/
│   │   ├── CardDemo.Modules.Transactions/
│   │   ├── CardDemo.Modules.Users/
│   │   ├── CardDemo.Modules.Reports/
│   │   └── CardDemo.Modules.Batch/
│   ├── Presentation/                 # Presentation layer
│   │   ├── CardDemo.WebAPI/          # REST API (entry point)
│   │   └── CardDemo.AdminPortal/     # Angular admin UI
│   └── Shared/                       # Shared libraries
│       ├── CardDemo.Shared.Kernel/
│       └── CardDemo.Shared.Contracts/
├── tests/                            # Test projects
│   ├── CardDemo.Domain.Tests/
│   ├── CardDemo.Application.Tests/
│   ├── CardDemo.Infrastructure.Tests/
│   ├── CardDemo.WebAPI.IntegrationTests/
│   ├── CardDemo.Architecture.Tests/
│   └── CardDemo.E2E.Tests/
├── tools/                            # Tooling and utilities
│   ├── CardDemo.Migration.Tool/      # VSAM to SQL migration
│   └── CardDemo.DataSeeder/          # Test data generation
├── deployment/                       # Infrastructure as Code
│   ├── terraform/                    # Terraform scripts
│   │   ├── modules/
│   │   ├── environments/
│   │   └── main.tf
│   ├── docker/                       # Dockerfiles
│   │   ├── Dockerfile.webapi
│   │   └── Dockerfile.batch
│   └── kubernetes/                   # K8s manifests (future)
├── scripts/                          # Build and deployment scripts
│   ├── build.ps1
│   ├── deploy-local.ps1
│   └── run-tests.ps1
├── .editorconfig                     # Code style configuration
├── .gitignore                        # Git ignore rules
├── .gitattributes                    # Git attributes
├── Directory.Build.props             # Global MSBuild properties
├── Directory.Packages.props          # Centralized package versioning
├── CardDemo.sln                      # Visual Studio solution file
├── global.json                       # Spring Boot SDK version pinning
├── nuget.config                      # NuGet package sources
└── README.md                         # Solution README
```

## Core Projects

### CardDemo.Domain

**Purpose**: Pure business logic, domain entities, value objects, domain events, repository interfaces.

**Dependencies**: None (pure domain layer, no external dependencies)

**Project Structure**:
```
CardDemo.Domain/
├── Aggregates/                       # Aggregate roots
│   ├── Account/
│   │   ├── Account.cs                # Account aggregate root
│   │   ├── AccountBalance.cs         # Entity within aggregate
│   │   └── AccountErrors.cs          # Domain errors
│   ├── Card/
│   │   ├── Card.cs
│   │   └── CardStatus.cs
│   ├── Customer/
│   │   ├── Customer.cs
│   │   └── Address.cs
│   ├── Transaction/
│   │   ├── Transaction.cs
│   │   ├── TransactionCategory.cs
│   │   └── CategoryBalance.cs
│   └── User/
│       ├── User.cs
│       └── Role.cs
├── ValueObjects/                     # Immutable value objects
│   ├── AccountId.cs
│   ├── CardNumber.cs
│   ├── Money.cs
│   ├── TransactionId.cs
│   ├── Email.cs
│   └── PhoneNumber.cs
├── Events/                           # Domain events
│   ├── AccountCreatedEvent.cs
│   ├── TransactionPostedEvent.cs
│   ├── CardActivatedEvent.cs
│   └── InterestCalculatedEvent.cs
├── Services/                         # Domain services
│   ├── IInterestCalculationService.cs
│   ├── IFraudDetectionService.cs
│   └── IAccountValidationService.cs
├── Repositories/                     # Repository interfaces
│   ├── IAccountRepository.cs
│   ├── ICardRepository.cs
│   ├── ITransactionRepository.cs
│   ├── ICustomerRepository.cs
│   └── IUserRepository.cs
├── Exceptions/                       # Domain exceptions
│   ├── DomainException.cs
│   ├── AccountNotFoundException.cs
│   ├── InsufficientCreditException.cs
│   └── InvalidCardStatusException.cs
├── Common/                           # Base classes and interfaces
│   ├── AggregateRoot.cs
│   ├── Entity.cs
│   ├── ValueObject.cs
│   ├── IDomainEvent.cs
│   └── IRepository.cs
└── CardDemo.Domain.csproj
```

**Key Characteristics**:
- No dependencies on Application, Infrastructure, or Presentation layers
- Rich domain model with business logic encapsulated in entities
- Value objects for immutability and domain concepts
- Domain events for communicating state changes
- Repository interfaces defined here, implemented in Infrastructure

### CardDemo.Application

**Purpose**: Application logic, use cases, commands, queries, DTOs, orchestration.

**Dependencies**: CardDemo.Domain

**Project Structure**:
```
CardDemo.Application/
├── Accounts/                         # Account module application logic
│   ├── Commands/
│   │   ├── CreateAccount/
│   │   │   ├── CreateAccountCommand.cs
│   │   │   ├── CreateAccountCommandHandler.cs
│   │   │   └── CreateAccountCommandValidator.cs
│   │   ├── UpdateAccount/
│   │   │   ├── UpdateAccountCommand.cs
│   │   │   ├── UpdateAccountCommandHandler.cs
│   │   │   └── UpdateAccountCommandValidator.cs
│   │   └── CalculateInterest/
│   │       ├── CalculateInterestCommand.cs
│   │       └── CalculateInterestCommandHandler.cs
│   ├── Queries/
│   │   ├── GetAccount/
│   │   │   ├── GetAccountQuery.cs
│   │   │   ├── GetAccountQueryHandler.cs
│   │   │   └── AccountDto.cs
│   │   ├── ListAccounts/
│   │   │   ├── ListAccountsQuery.cs
│   │   │   ├── ListAccountsQueryHandler.cs
│   │   │   └── AccountListItemDto.cs
│   │   └── GetAccountBalance/
│   │       ├── GetAccountBalanceQuery.cs
│   │       └── GetAccountBalanceQueryHandler.cs
│   └── EventHandlers/
│       ├── AccountCreatedEventHandler.cs
│       └── InterestCalculatedEventHandler.cs
├── Cards/                            # Card module application logic
│   ├── Commands/
│   │   ├── CreateCard/
│   │   ├── UpdateCard/
│   │   └── ActivateCard/
│   ├── Queries/
│   │   ├── GetCard/
│   │   ├── ListCards/
│   │   └── SearchCards/
│   └── EventHandlers/
│       └── CardActivatedEventHandler.cs
├── Transactions/                     # Transaction module application logic
│   ├── Commands/
│   │   ├── PostTransaction/
│   │   ├── VoidTransaction/
│   │   └── ProcessDailyBatch/
│   ├── Queries/
│   │   ├── GetTransaction/
│   │   ├── ListTransactions/
│   │   └── GetCategoryBalances/
│   └── EventHandlers/
│       └── TransactionPostedEventHandler.cs
├── Users/                            # User management application logic
│   ├── Commands/
│   │   ├── CreateUser/
│   │   ├── UpdateUser/
│   │   └── DeleteUser/
│   └── Queries/
│       ├── GetUser/
│       └── ListUsers/
├── Authentication/                   # Authentication application logic
│   ├── Commands/
│   │   ├── Login/
│   │   │   ├── LoginCommand.cs
│   │   │   ├── LoginCommandHandler.cs
│   │   │   ├── LoginCommandValidator.cs
│   │   │   └── LoginResultDto.cs
│   │   ├── Logout/
│   │   └── RefreshToken/
│   └── Queries/
│       └── GetCurrentUser/
├── Common/                           # Shared application logic
│   ├── Behaviours/                   # Axon Framework pipeline behaviors
│   │   ├── ValidationBehavior.cs
│   │   ├── LoggingBehavior.cs
│   │   ├── TransactionBehavior.cs
│   │   └── PerformanceBehavior.cs
│   ├── Exceptions/
│   │   ├── ValidationException.cs
│   │   └── NotFoundException.cs
│   ├── Interfaces/
│   │   ├── ICurrentUserService.cs
│   │   ├── IDateTime.cs
│   │   └── IEventPublisher.cs
│   ├── Mappings/
│   │   └── MappingProfile.cs         # AutoMapper profiles
│   └── Models/
│       ├── PaginatedList.cs
│       └── Result.cs                 # Result pattern for errors
└── CardDemo.Application.csproj
```

**Key NuGet Packages**:
- Axon Framework (CQRS mediator)
- FluentValidation (command/query validation)
- AutoMapper (entity ↔ DTO mapping)

**Key Characteristics**:
- Commands modify state, Queries read data
- Each command/query has dedicated handler
- FluentValidation for input validation
- Axon Framework pipeline behaviors for cross-cutting concerns (validation, logging, transactions)

## Infrastructure Projects

### CardDemo.Infrastructure

**Purpose**: Data access (EF Core), external services, cross-cutting concerns.

**Dependencies**: CardDemo.Domain, CardDemo.Application

**Project Structure**:
```
CardDemo.Infrastructure/
├── Persistence/                      # EF Core data access
│   ├── CardDemoDbContext.cs          # Main DbContext
│   ├── Configurations/               # Entity configurations
│   │   ├── AccountConfiguration.cs
│   │   ├── CardConfiguration.cs
│   │   ├── TransactionConfiguration.cs
│   │   └── UserConfiguration.cs
│   ├── Repositories/                 # Repository implementations
│   │   ├── AccountRepository.cs
│   │   ├── CardRepository.cs
│   │   ├── TransactionRepository.cs
│   │   └── UserRepository.cs
│   ├── Interceptors/                 # EF Core interceptors
│   │   ├── AuditInterceptor.cs       # Automatic audit fields
│   │   └── SoftDeleteInterceptor.cs
│   └── Migrations/                   # EF Core migrations
│       ├── 20250120_InitialCreate.cs
│       ├── 20250121_AddAccountTables.cs
│       └── ...
├── Services/                         # Infrastructure services
│   ├── DateTimeService.cs            # IDateTime implementation
│   ├── CurrentUserService.cs         # ICurrentUserService implementation
│   ├── EmailService.cs               # Email sending
│   └── FileStorageService.cs         # Azure Blob Storage
├── Identity/                         # ASPSpring Boot Core Identity
│   ├── ApplicationUser.cs            # Extended Identity user
│   ├── ApplicationRole.cs
│   ├── IdentityService.cs
│   └── JwtTokenGenerator.cs
├── Caching/                          # Caching implementations
│   ├── RedisCacheService.cs
│   └── MemoryCacheService.cs
├── Security/                         # Security services
│   ├── PasswordHasher.cs
│   ├── DataProtectionService.cs      # Encrypt/decrypt sensitive data
│   └── AuditLogger.cs
└── CardDemo.Infrastructure.csproj
```

**Key NuGet Packages**:
- Microsoft.EntityFrameworkCore.SqlServer
- Microsoft.AspNetCore.Identity.EntityFrameworkCore
- StackExchange.Redis
- Azure.Storage.Blobs

### CardDemo.Infrastructure.Messaging

**Purpose**: Event publishing, message handling, Azure Service Bus integration.

**Dependencies**: CardDemo.Domain, CardDemo.Application

**Project Structure**:
```
CardDemo.Infrastructure.Messaging/
├── Events/                           # Integration events (serializable)
│   ├── AccountCreatedIntegrationEvent.cs
│   ├── TransactionPostedIntegrationEvent.cs
│   └── CardActivatedIntegrationEvent.cs
├── Publishers/
│   ├── EventPublisher.cs             # IEventPublisher implementation
│   └── ServiceBusPublisher.cs
├── Consumers/                        # Event consumers
│   ├── AccountCreatedConsumer.cs
│   ├── TransactionPostedConsumer.cs
│   └── InterestCalculationConsumer.cs
├── Configuration/
│   ├── ServiceBusConfiguration.cs
│   └── TopicConfiguration.cs
└── CardDemo.Infrastructure.Messaging.csproj
```

**Key NuGet Packages**:
- Azure.Messaging.ServiceBus
- MassTransit (optional, for higher-level abstractions)

## Presentation Projects

### CardDemo.WebAPI

**Purpose**: REST API entry point, controllers, middleware, API contracts.

**Dependencies**: CardDemo.Application, CardDemo.Infrastructure, CardDemo.Infrastructure.Messaging

**Project Structure**:
```
CardDemo.WebAPI/
├── Controllers/                      # API controllers
│   ├── AccountsController.cs
│   ├── CardsController.cs
│   ├── TransactionsController.cs
│   ├── UsersController.cs
│   ├── AuthenticationController.cs
│   └── ReportsController.cs
├── Middleware/                       # Custom middleware
│   ├── ExceptionHandlingMiddleware.cs
│   ├── RequestLoggingMiddleware.cs
│   └── CorrelationIdMiddleware.cs
├── Filters/                          # Action filters
│   ├── ValidateModelAttribute.cs
│   └── ApiKeyAuthorizationFilter.cs
├── Extensions/                       # Service registration extensions
│   ├── ServiceCollectionExtensions.cs
│   ├── ApplicationBuilderExtensions.cs
│   └── ConfigurationExtensions.cs
├── HealthChecks/                     # Health check implementations
│   ├── DatabaseHealthCheck.cs
│   ├── ServiceBusHealthCheck.cs
│   └── RedisHealthCheck.cs
├── Configuration/                    # Startup configuration
│   ├── SwaggerConfiguration.cs
│   ├── AuthenticationConfiguration.cs
│   └── CorsConfiguration.cs
├── Program.cs                        # Application entry point
├── appsettings.json                  # Configuration (dev settings)
├── appsettings.Development.json
├── appsettings.Production.json
├── Dockerfile                        # Docker image definition
└── CardDemo.WebAPI.csproj
```

**Key NuGet Packages**:
- Microsoft.AspNetCore.Authentication.JwtBearer
- Swashbuckle.AspNetCore (Swagger/OpenAPI)
- Serilog.AspNetCore
- Microsoft.Extensions.Diagnostics.HealthChecks

**Program.cs Structure** (Minimal API style):
```csharp
var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddApplicationServices();      // Application layer
builder.Services.AddInfrastructureServices(builder.Configuration);
builder.Services.AddAuthentication(builder.Configuration);
builder.Services.AddAuthorization();
builder.Services.AddControllers();
builder.Services.AddSwaggerGen();
builder.Services.AddHealthChecks();

var app = builder.Build();

// Configure HTTP request pipeline
app.UseSwagger();
app.UseSwaggerUI();
app.UseHttpsRedirection();
app.UseAuthentication();
app.UseAuthorization();
app.MapControllers();
app.MapHealthChecks("/health");

app.Run();
```

### CardDemo.AdminPortal

**Purpose**: Angular admin UI for system administration.

**Dependencies**: CardDemo.Application (via WebAPI HTTP calls)

**Project Structure**:
```
CardDemo.AdminPortal/
├── Pages/                            # Angular components
│   ├── Index.razor
│   ├── Login.razor
│   ├── Accounts/
│   │   ├── AccountList.razor
│   │   ├── AccountDetails.razor
│   │   └── AccountEdit.razor
│   ├── Cards/
│   │   ├── CardList.razor
│   │   └── CardDetails.razor
│   ├── Users/
│   │   ├── UserList.razor
│   │   ├── UserAdd.razor
│   │   └── UserEdit.razor
│   └── Reports/
│       ├── TransactionReport.razor
│       └── StatementGeneration.razor
├── Components/                       # Reusable components
│   ├── DataGrid.razor
│   ├── Pagination.razor
│   └── ConfirmDialog.razor
├── Services/                         # Client-side services
│   ├── ApiClient.cs                  # HTTP client wrapper
│   ├── AuthenticationService.cs
│   └── NotificationService.cs
├── wwwroot/                          # Static files
│   ├── css/
│   │   └── site.css
│   ├── js/
│   │   └── site.js
│   └── favicon.ico
├── _Imports.razor                    # Global imports
├── App.razor                         # App component
├── Program.cs
└── CardDemo.AdminPortal.csproj
```

## Module Projects (Bounded Contexts)

**Note**: Modules are **logical** groupings within the monolith, not separate deployments initially. They are organized as folders within Application and Domain projects, **not** separate projects, to avoid excessive project proliferation while maintaining clear boundaries.

**Alternative Structure** (if modules grow large enough to warrant separation):
```
CardDemo.Modules.Accounts/
├── Domain/
│   ├── Account.cs
│   └── AccountRepository.cs
├── Application/
│   ├── Commands/
│   └── Queries/
├── Infrastructure/
│   └── Persistence/
└── CardDemo.Modules.Accounts.csproj
```

This vertical slice architecture per module can be adopted later if complexity warrants it.

## Shared Projects

### CardDemo.Shared.Kernel

**Purpose**: Shared domain primitives, base classes, common interfaces.

**Dependencies**: None

**Project Structure**:
```
CardDemo.Shared.Kernel/
├── Common/
│   ├── Entity.cs
│   ├── ValueObject.cs
│   ├── AggregateRoot.cs
│   └── IDomainEvent.cs
├── Interfaces/
│   ├── IRepository.cs
│   ├── IUnitOfWork.cs
│   └── IAuditableEntity.cs
├── Exceptions/
│   ├── DomainException.cs
│   └── ValidationException.cs
└── CardDemo.Shared.Kernel.csproj
```

### CardDemo.Shared.Contracts

**Purpose**: API contracts (DTOs) shared between WebAPI and clients.

**Dependencies**: None

**Project Structure**:
```
CardDemo.Shared.Contracts/
├── Accounts/
│   ├── AccountDto.cs
│   ├── CreateAccountRequest.cs
│   └── UpdateAccountRequest.cs
├── Cards/
│   ├── CardDto.cs
│   └── UpdateCardRequest.cs
├── Transactions/
│   ├── TransactionDto.cs
│   └── PostTransactionRequest.cs
├── Common/
│   ├── PaginatedResponse.cs
│   └── ErrorResponse.cs
└── CardDemo.Shared.Contracts.csproj
```

## Test Projects

### Unit Test Projects

**CardDemo.Domain.Tests**:
```
CardDemo.Domain.Tests/
├── Aggregates/
│   ├── AccountTests.cs
│   ├── CardTests.cs
│   └── TransactionTests.cs
├── ValueObjects/
│   ├── AccountIdTests.cs
│   ├── MoneyTests.cs
│   └── CardNumberTests.cs
├── Services/
│   └── InterestCalculationServiceTests.cs
└── CardDemo.Domain.Tests.csproj
```

**CardDemo.Application.Tests**:
```
CardDemo.Application.Tests/
├── Accounts/
│   ├── Commands/
│   │   ├── CreateAccountCommandHandlerTests.cs
│   │   └── UpdateAccountCommandHandlerTests.cs
│   └── Queries/
│       └── GetAccountQueryHandlerTests.cs
├── Cards/
│   └── Commands/
│       └── UpdateCardCommandHandlerTests.cs
└── CardDemo.Application.Tests.csproj
```

### Integration Test Projects

**CardDemo.WebAPI.IntegrationTests**:
```
CardDemo.WebAPI.IntegrationTests/
├── Controllers/
│   ├── AccountsControllerTests.cs
│   ├── CardsControllerTests.cs
│   └── AuthenticationControllerTests.cs
├── Infrastructure/
│   ├── CustomWebApplicationFactory.cs
│   └── TestFixture.cs
└── CardDemo.WebAPI.IntegrationTests.csproj
```

### Architecture Test Project

**CardDemo.Architecture.Tests**:
```
CardDemo.Architecture.Tests/
├── ArchitectureTests.cs              # Enforce Clean Architecture rules
├── LayerDependencyTests.cs
├── NamingConventionTests.cs
└── CardDemo.Architecture.Tests.csproj
```

**Example Architecture Test**:
```csharp
[Fact]
public void Domain_Should_Not_Depend_On_Application()
{
    var domainAssembly = typeof(Account).Assembly;
    var applicationAssembly = typeof(CreateAccountCommand).Assembly;
    
    var result = Types.InAssembly(domainAssembly)
        .Should()
        .NotHaveDependencyOn(applicationAssembly.GetName().Name)
        .GetResult();
    
    result.IsSuccessful.Should().BeTrue();
}
```

## File Naming Conventions

### Project Files
- **Format**: `{CompanyName}.{ProductName}.{Layer}.csproj`
- **Example**: `CardDemo.Application.csproj`

### Class Files
- **Format**: `{ClassName}.cs` (one class per file, file name matches class name)
- **Example**: `Account.cs`, `CreateAccountCommand.cs`

### Commands and Queries
- **Commands**: `{Verb}{Entity}Command.cs`
  - Example: `CreateAccountCommand.cs`, `UpdateCardCommand.cs`, `PostTransactionCommand.cs`
- **Command Handlers**: `{CommandName}Handler.cs`
  - Example: `CreateAccountCommandHandler.cs`
- **Command Validators**: `{CommandName}Validator.cs`
  - Example: `CreateAccountCommandValidator.cs`

- **Queries**: `Get{Entity}Query.cs`, `List{Entities}Query.cs`
  - Example: `GetAccountQuery.cs`, `ListTransactionsQuery.cs`
- **Query Handlers**: `{QueryName}Handler.cs`
  - Example: `GetAccountQueryHandler.cs`

### DTOs
- **Format**: `{Entity}Dto.cs`
- **Example**: `AccountDto.cs`, `TransactionDto.cs`
- **List Items**: `{Entity}ListItemDto.cs` (for list views)

### API Requests/Responses
- **Requests**: `{Verb}{Entity}Request.cs`
  - Example: `CreateAccountRequest.cs`, `UpdateCardRequest.cs`
- **Responses**: `{Entity}Response.cs`
  - Example: `AccountResponse.cs`, `TransactionListResponse.cs`

### Test Files
- **Format**: `{ClassUnderTest}Tests.cs`
- **Example**: `AccountTests.cs`, `CreateAccountCommandHandlerTests.cs`

### Configuration Files
- **Format**: `{Entity}Configuration.cs` (EF Core)
- **Example**: `AccountConfiguration.cs`, `TransactionConfiguration.cs`

## Namespace Conventions

- **Domain**: `CardDemo.Domain.{AggregateOrConcept}`
  - Example: `CardDemo.Domain.Aggregates.Account`, `CardDemo.Domain.ValueObjects`

- **Application**: `CardDemo.Application.{Module}.{Commands|Queries}.{Operation}`
  - Example: `CardDemo.Application.Accounts.Commands.CreateAccount`

- **Infrastructure**: `CardDemo.Infrastructure.{Concern}`
  - Example: `CardDemo.Infrastructure.Persistence`, `CardDemo.Infrastructure.Identity`

- **WebAPI**: `CardDemo.WebAPI.{Controllers|Middleware|Filters}`
  - Example: `CardDemo.WebAPI.Controllers`, `CardDemo.WebAPI.Middleware`

## Configuration Management

### Configuration Sources (Priority Order)
1. Command-line arguments (highest priority)
2. Environment variables
3. Azure Key Vault (production secrets)
4. `appsettings.{Environment}.json`
5. `appsettings.json` (lowest priority)

### appsettings.json Structure
```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Server=...;Database=...;", 
    "RedisConnection": "...",
    "ServiceBusConnection": "..."
  },
  "JwtSettings": {
    "Issuer": "https://api.carddemo.com",
    "Audience": "carddemo-api",
    "SecretKey": "{KeyVault:JwtSecretKey}",
    "ExpirationMinutes": 60
  },
  "AzureKeyVault": {
    "VaultUri": "https://carddemo-kv.vault.azure.net/"
  },
  "ApplicationInsights": {
    "InstrumentationKey": "{KeyVault:AppInsightsKey}"
  },
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning"
    }
  }
}
```

### Environment-Specific Settings
- **Development**: Local SQL Server, local Redis, no Key Vault
- **Staging**: Azure SQL, Azure Redis, Key Vault, limited scale
- **Production**: Azure SQL (geo-replicated), Azure Redis (HA), Key Vault, full scale

## Build Configuration

### Directory.Build.props (Root)
```xml
<Project>
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <LangVersion>14</LangVersion>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
    <NoWarn>$(NoWarn);1591</NoWarn> <!-- Suppress XML comment warnings -->
  </PropertyGroup>

  <PropertyGroup>
    <Authors>CardDemo Team</Authors>
    <Company>AE</Company>
    <Product>CardDemo Modernized</Product>
    <Copyright>Copyright © 2025 AE</Copyright>
  </PropertyGroup>
</Project>
```

### Directory.Packages.props (Centralized Package Management)
```xml
<Project>
  <PropertyGroup>
    <ManagePackageVersionsCentrally>true</ManagePackageVersionsCentrally>
  </PropertyGroup>

  <ItemGroup>
    <!-- Core Framework -->
    <PackageVersion Include="Microsoft.AspNetCore.OpenApi" Version="10.0.0" />
    <PackageVersion Include="Swashbuckle.AspNetCore" Version="6.9.0" />
    
    <!-- Architecture -->
    <PackageVersion Include="Axon Framework" Version="12.4.0" />
    <PackageVersion Include="FluentValidation" Version="11.10.0" />
    <PackageVersion Include="AutoMapper" Version="13.0.0" />
    
    <!-- Data Access -->
    <PackageVersion Include="Microsoft.EntityFrameworkCore.SqlServer" Version="10.0.0" />
    <PackageVersion Include="Microsoft.EntityFrameworkCore.Tools" Version="10.0.0" />
    
    <!-- Testing -->
    <PackageVersion Include="xunit" Version="2.9.0" />
    <PackageVersion Include="Moq" Version="4.20.0" />
    <PackageVersion Include="FluentAssertions" Version="6.12.0" />
  </ItemGroup>
</Project>
```

## Docker Configuration

### Dockerfile (Multi-Stage Build)
```dockerfile
# Build stage
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /src

# Copy solution and project files
COPY CardDemo.sln .
COPY src/Core/CardDemo.Domain/CardDemo.Domain.csproj src/Core/CardDemo.Domain/
COPY src/Core/CardDemo.Application/CardDemo.Application.csproj src/Core/CardDemo.Application/
COPY src/Infrastructure/CardDemo.Infrastructure/CardDemo.Infrastructure.csproj src/Infrastructure/CardDemo.Infrastructure/
COPY src/Presentation/CardDemo.WebAPI/CardDemo.WebAPI.csproj src/Presentation/CardDemo.WebAPI/

# Restore dependencies
RUN dotnet restore

# Copy remaining source code
COPY . .

# Build application
WORKDIR /src/src/Presentation/CardDemo.WebAPI
RUN dotnet build -c Release -o /app/build

# Publish stage
FROM build AS publish
RUN dotnet publish -c Release -o /app/publish /p:UseAppHost=false

# Runtime stage
FROM mcr.microsoft.com/dotnet/aspnet:10.0 AS final
WORKDIR /app

# Create non-root user
RUN addgroup --system --gid 1000 appuser \
    && adduser --system --uid 1000 --ingroup appuser --shell /bin/sh appuser

# Copy published application
COPY --from=publish /app/publish .

# Change ownership
RUN chown -R appuser:appuser /app

# Switch to non-root user
USER appuser

# Configure application
EXPOSE 8080
EXPOSE 8081
ENV ASPNETCORE_URLS=http://+:8080
ENV ASPNETCORE_ENVIRONMENT=Production

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

ENTRYPOINT ["dotnet", "CardDemo.WebAPI.dll"]
```

## Conclusion

This solution structure provides:
- **Clear Separation**: Distinct layers with well-defined responsibilities
- **Scalability**: Modular design enables growth and potential microservices extraction
- **Testability**: Isolated layers and dependency injection enable comprehensive testing
- **Maintainability**: Consistent naming, organization, and conventions
- **Developer Experience**: Logical structure, easy navigation, minimal cognitive load

The structure balances pragmatism (modular monolith) with modern architectural principles (Clean Architecture, DDD, CQRS), providing a solid foundation for the CardDemo modernization.

---

**Related Documents**:
- Architecture Overview: `overview.md`
- Technology Stack: `technology-stack.md`
- Coding Standards: `guidelines/coding-standards.md`
- ADR-008: Clean Architecture with DDD Tactical Patterns
