# Technology Stack

**Document Version**: 1.0  
**Last Updated**: 2025-11-20  
**Status**: Draft  
**Owner**: Software Architect

## Overview

This document defines the complete technology stack for the modernized CardDemo application, including programming languages, frameworks, libraries, cloud services, and development tools. All selections are based on:

- **Modern Best Practices**: Industry-standard, actively maintained technologies
- **Microsoft Ecosystem**: Leverage .NET platform for consistency and support
- **Azure Cloud-Native**: First-party Azure services for reliability and integration
- **Team Productivity**: Developer-friendly tools with strong community support
- **Long-Term Viability**: LTS versions and stable technologies

## Core Platform

### .NET Runtime and SDK

| Component | Version | Rationale |
|-----------|---------|-----------|
| **.NET SDK** | .NET 10.0 | Latest LTS (Long-Term Support) release, supported until 2027, best performance, C# 14 features |
| **C# Language** | C# 14 | Latest language version with .NET 10, nullable reference types, records, pattern matching |
| **ASP.NET Core** | 10.0 | Web framework for APIs and web applications, high performance, cross-platform |

**Why .NET 10 LTS**:
- 3 years of support (2024-2027) vs. 18 months for non-LTS
- Production-ready and stable
- Best performance (compared to .NET 8 LTS)
- Modern C# features (required types, record types, init-only properties)
- Native AOT support for smaller deployments (future option)

## Application Framework

### Web Framework

| Component | Version | Purpose |
|-----------|---------|---------|
| **ASP.NET Core Minimal APIs** | 10.0 | RESTful API endpoints, lightweight, high performance |
| **ASP.NET Core MVC** | 10.0 | Full-featured controllers (if needed for complex routing) |
| **Blazor Server** | 10.0 | Admin portal UI, real-time server-side rendering |
| **SignalR** | 10.0 | Real-time communication (notifications, live updates) |

**API Approach**: Minimal APIs for simplicity and performance, MVC controllers for complex scenarios.

### Architecture Patterns & Libraries

| Library | Version | Purpose |
|---------|---------|---------|
| **MediatR** | 12.x | CQRS implementation, command/query separation, request/response mediator |
| **FluentValidation** | 11.x | Input validation, declarative validation rules for commands/queries |
| **AutoMapper** | 13.x | Object-to-object mapping (Entity ↔ DTO), reduces boilerplate |
| **Polly** | 8.x | Resilience patterns (retry, circuit breaker, timeout, fallback) |

**CQRS with MediatR**:
- Commands: `IRequest<Result>` for operations that modify state
- Queries: `IRequest<TResponse>` for operations that read data
- Handlers: `IRequestHandler<TRequest, TResponse>` for processing logic
- Pipeline behaviors: Validation, logging, transaction management

## Data Access

### ORM and Database

| Component | Version | Purpose |
|-----------|---------|---------|
| **Entity Framework Core** | 10.0 | Object-relational mapping, LINQ queries, migrations |
| **EF Core SQL Server Provider** | 10.0 | Azure SQL Database connectivity |
| **EF Core Design** | 10.0 | Migrations, scaffolding, design-time tools |

**EF Core Features Used**:
- **Code-First**: Define entities in C#, generate database schema
- **Migrations**: Version-controlled schema changes
- **Fluent API**: Configure entity mappings (relationships, indexes, constraints)
- **Query Splitting**: Optimize queries with multiple includes
- **Compiled Queries**: Cache query plans for performance
- **Change Tracking**: Automatic dirty detection for updates

### Database

| Component | Tier/SKU | Purpose |
|-----------|----------|---------|
| **Azure SQL Database** | Standard S3 (100 DTU) | Primary data store, ACID transactions, relational data |
| **SQL Server Management Studio (SSMS)** | Latest | Database administration and query tool |

**Why Azure SQL Database**:
- Fully managed (automated backups, patching, HA)
- Built-in security (TDE, Always Encrypted, auditing)
- Elastic scaling (DTU or vCore models)
- Geo-replication for DR
- Query Performance Insights
- Integration with Azure ecosystem

**Initial Sizing**: Standard S3 (100 DTU, 250 GB storage) - sufficient for CardDemo's estimated load, can scale up/down as needed.

### Caching

| Component | Tier | Purpose |
|-----------|------|---------|
| **Azure Redis Cache** | Standard 1 GB | Distributed cache, session storage, frequently accessed data |
| **IMemoryCache (In-Process)** | N/A | Application-level caching for reference data |

**Caching Strategy**:
- **Redis**: User sessions, account lookup cache (5-minute TTL), card details (10-minute TTL)
- **IMemoryCache**: Transaction category reference data, static configuration (1-hour TTL)
- **Cache-aside pattern**: Check cache → if miss, fetch from DB and populate cache

## Messaging and Events

### Event Bus

| Component | Tier | Purpose |
|-----------|------|---------|
| **Azure Service Bus** | Standard | Asynchronous messaging, event distribution, reliable delivery |

**Service Bus Configuration**:
- **Topics**: `account-events`, `transaction-events`, `card-events`, `audit-events`
- **Subscriptions**: One per consumer (e.g., `interest-calculator-subscription`, `audit-logger-subscription`)
- **Message TTL**: 14 days (retain for debugging and replay)
- **Dead Letter Queue**: Enabled for failed message handling

**Event-Driven Patterns**:
- **Domain Events**: Published when aggregate state changes (AccountCreatedEvent, TransactionPostedEvent)
- **Integration Events**: Serialized as JSON, sent to Service Bus
- **Event Handlers**: Subscribe to topics, process events asynchronously
- **Idempotency**: Track processed message IDs to prevent duplicate processing

## Security and Identity

### Authentication and Authorization

| Component | Purpose |
|-----------|---------|
| **ASP.NET Core Identity** | User management (registration, login, password hashing, roles) |
| **JWT Bearer Authentication** | Token-based authentication for API requests |
| **Azure Active Directory / Entra ID** | Enterprise SSO integration (future capability) |
| **Azure Key Vault** | Secrets management (connection strings, API keys, certificates) |

**Authentication Flow**:
1. User submits credentials to `/api/auth/login`
2. ASP.NET Core Identity validates credentials
3. Generate JWT token with claims (user ID, roles, expiration)
4. Client includes token in `Authorization: Bearer <token>` header
5. ASP.NET Core JWT middleware validates token signature and expiration
6. User principal populated with claims for authorization

**JWT Configuration**:
- **Issuer**: `https://api.carddemo.com`
- **Audience**: `carddemo-api`
- **Signing Key**: Stored in Azure Key Vault, RSA-256 algorithm
- **Token Lifetime**: 60 minutes (sliding expiration with refresh tokens)
- **Claims**: `sub` (user ID), `role` (user role), `name` (display name)

**Authorization Policies**:
- **Role-Based**: `[Authorize(Roles = "Administrator")]`
- **Policy-Based**: `[Authorize(Policy = "RequireApprovedUser")]`
- **Claims-Based**: `[Authorize(Policy = "RequireDepartment:CustomerService")]`

### Data Protection

| Component | Purpose |
|-----------|---------|
| **ASP.NET Core Data Protection API** | Encrypt/decrypt sensitive data (SSN, card numbers) |
| **Azure SQL TDE** | Transparent database encryption at rest |
| **Azure Key Vault** | Key storage, key rotation, HSM-backed keys |

**Sensitive Data Encryption**:
- **SSN**: Encrypted in application layer before storage, decrypted on read (attribute-based)
- **Card Numbers**: Masked in logs and non-privileged views, full number only for authorized roles
- **Passwords**: Hashed with bcrypt (cost factor 12), never stored plaintext

## Cloud Services (Azure)

### Compute

| Service | Configuration | Purpose |
|---------|---------------|---------|
| **Azure Container Apps** | 1-10 instances, auto-scale | Hosting for ASP.NET Core application, serverless containers |
| **Azure Functions** | Consumption plan | Scheduled batch jobs (interest calculation, cycle reset) |

**Container Apps Configuration**:
- **Container Image**: Docker image from Azure Container Registry
- **Scaling**: 
  - Min replicas: 1 (always-on)
  - Max replicas: 10 (burst capacity)
  - Scale rules: CPU > 70%, Memory > 80%, HTTP request rate > 1000/min
- **Health Checks**: 
  - Liveness: `/health/live`
  - Readiness: `/health/ready`
  - Startup: `/health/startup`

### API Gateway

| Service | Configuration | Purpose |
|---------|---------------|---------|
| **Azure API Management** | Standard tier | API gateway, rate limiting, caching, routing, transformation |

**APIM Policies**:
- **Authentication**: Validate JWT tokens
- **Rate Limiting**: 1000 requests per minute per client
- **Response Caching**: Cache GET requests for 60 seconds
- **CORS**: Allow specific origins (admin portal domain)
- **Request Transformation**: Header injection (correlation ID)
- **Routing**: `/api/v1/*` → Container Apps backend

### Storage

| Service | Configuration | Purpose |
|---------|---------------|---------|
| **Azure Blob Storage** | Standard LRS | File storage (statements, reports, exports, document uploads) |

### Monitoring and Observability

| Service | Configuration | Purpose |
|---------|---------------|---------|
| **Azure Application Insights** | Log Analytics-based | APM, distributed tracing, custom metrics, logs |
| **Azure Log Analytics** | 30-day retention | Log aggregation, KQL queries, alerts |
| **Azure Monitor** | Alerts enabled | Infrastructure monitoring, alerting, dashboards |

**Application Insights Configuration**:
- **Instrumentation Key**: Stored in Key Vault
- **Sampling**: Adaptive sampling (5 requests per second ingestion rate)
- **Telemetry**:
  - Requests: All HTTP requests with response times
  - Dependencies: SQL, Redis, Service Bus calls
  - Exceptions: Unhandled exceptions with stack traces
  - Custom Events: Business metrics (transaction posted, account created)
  - Custom Metrics: TPS, auth success rate, balance update latency

## Development Tools

### IDE and Editors

| Tool | Version | Purpose |
|------|---------|---------|
| **Visual Studio 2022** | 17.13+ | Primary IDE for .NET development (Windows) |
| **Visual Studio Code** | Latest | Lightweight editor (cross-platform), Blazor WASM development |
| **JetBrains Rider** | 2024.3+ | Alternative IDE (cross-platform), strong refactoring tools |

**Recommended Extensions**:
- Visual Studio: ReSharper, CodeMaid, Visual Studio Spell Checker
- VS Code: C# Dev Kit, Azure Tools, Docker, REST Client

### Source Control and CI/CD

| Tool | Purpose |
|------|---------|
| **Git** | Version control |
| **GitHub** | Source code hosting, pull requests, code reviews |
| **GitHub Actions** | CI/CD pipelines (build, test, deploy) |

**GitHub Actions Workflows**:
1. **Build & Test** (on every push/PR):
   - Restore NuGet packages
   - Build solution
   - Run unit tests with code coverage
   - Run architecture tests (ArchUnit.NET)
   - SonarQube analysis
   - Publish code coverage report

2. **Deploy to Dev** (on merge to `develop` branch):
   - Build Docker image
   - Push to Azure Container Registry
   - Deploy to Container Apps (dev environment)
   - Run smoke tests

3. **Deploy to Production** (on merge to `main` branch):
   - Deploy to Container Apps (production environment)
   - Run E2E tests
   - Notify team on Slack/Teams

### Containerization

| Tool | Version | Purpose |
|------|---------|---------|
| **Docker Desktop** | 4.36+ | Local containerization, testing, development |
| **Azure Container Registry** | Standard tier | Private Docker registry for images |

**Dockerfile Strategy**:
- Multi-stage build (SDK stage → runtime stage)
- Base image: `mcr.microsoft.com/dotnet/aspnet:10.0`
- Non-root user for security
- Health check endpoint configured

## Testing Frameworks

### Unit Testing

| Library | Version | Purpose |
|---------|---------|---------|
| **xUnit** | 2.9.x | Test framework, test runner |
| **FluentAssertions** | 6.x | Fluent assertion library, readable test assertions |
| **Moq** | 4.x | Mocking framework for unit tests (isolate dependencies) |
| **AutoFixture** | 4.x | Test data generation, reduce boilerplate |

**Testing Patterns**:
- **AAA Pattern**: Arrange → Act → Assert
- **One Assert Per Test**: Focus on single behavior
- **Test Naming**: `MethodName_Scenario_ExpectedResult`

### Integration Testing

| Library | Version | Purpose |
|---------|---------|---------|
| **Microsoft.AspNetCore.Mvc.Testing** | 10.0 | In-memory test server (WebApplicationFactory) |
| **Testcontainers** | 3.x | Docker containers for integration tests (SQL Server, Redis) |
| **Respawn** | 6.x | Database cleanup between tests |

**Integration Test Scope**:
- API endpoint testing (HTTP requests → responses)
- Database operations (EF Core repository tests)
- Event publishing/subscription (Service Bus tests)

### Architecture Testing

| Library | Version | Purpose |
|---------|---------|---------|
| **ArchUnit.NET** | 0.x | Enforce architectural rules (layer dependencies, naming conventions) |

**Architecture Test Rules**:
- Domain layer has no dependencies on Infrastructure or Application layers
- Controllers only depend on MediatR (no direct repository access)
- Entities and Value Objects are in Domain layer
- Repository interfaces in Domain, implementations in Infrastructure

### E2E Testing

| Tool | Purpose |
|------|---------|
| **Playwright** | Browser automation for E2E testing (future) |
| **SpecFlow** | BDD (Behavior-Driven Development) with Gherkin syntax (optional) |

## Code Quality Tools

| Tool | Purpose |
|------|---------|
| **SonarQube** | Static code analysis, code smells, security vulnerabilities |
| **.NET Analyzers** | Built-in Roslyn analyzers (enabled by default) |
| **StyleCop.Analyzers** | Code style enforcement (naming, formatting) |
| **EditorConfig** | Consistent code formatting across team |

**Code Quality Gates**:
- Code coverage > 80%
- No critical or blocker issues in SonarQube
- All StyleCop rules pass
- No compiler warnings

## Logging

| Library | Version | Purpose |
|---------|---------|---------|
| **Serilog** | 4.x | Structured logging, rich log context |
| **Serilog.AspNetCore** | 10.x | ASP.NET Core integration, request logging |
| **Serilog.Sinks.ApplicationInsights** | 4.x | Send logs to Application Insights |

**Logging Configuration**:
- **Log Levels**: Debug, Information, Warning, Error, Fatal
- **Structured Logging**: JSON format with properties (`{UserId}`, `{AccountId}`)
- **Sinks**: 
  - Console (development)
  - Application Insights (production)
  - File (optional, rolling file)

**Logging Best Practices**:
- Use semantic logging: `_logger.LogInformation("Account {AccountId} updated by {UserId}", accountId, userId)`
- Include correlation IDs for request tracing
- Avoid logging sensitive data (passwords, full SSNs, full card numbers)
- Use scopes for contextual logging

## Documentation

| Tool | Purpose |
|------|---------|
| **Swagger/OpenAPI** | API documentation (auto-generated from controllers) |
| **Swashbuckle** | OpenAPI generation for ASP.NET Core |
| **ReDoc** | Alternative API documentation UI (cleaner than Swagger UI) |
| **DocFX** | Generate documentation website from XML comments |
| **Markdown** | Architecture documentation, ADRs, guides |

**API Documentation**:
- **Swagger UI**: Available at `/swagger` (development only)
- **OpenAPI Spec**: Available at `/swagger/v1/swagger.json`
- **XML Comments**: All public APIs documented with `<summary>`, `<param>`, `<returns>`

## Infrastructure as Code (IaC)

| Tool | Purpose |
|------|---------|
| **Terraform** | Infrastructure provisioning (Azure resources) |
| **Azure CLI** | Command-line resource management, scripting |

**Terraform Modules**:
- `azure-sql-database`: SQL Database, firewall rules, connection strings
- `azure-container-apps`: Container Apps Environment, apps, ingress
- `azure-service-bus`: Namespace, topics, subscriptions
- `azure-apim`: API Management instance, APIs, policies
- `azure-monitoring`: Application Insights, Log Analytics, alert rules

## Version Management

| Component | Version Strategy |
|-----------|------------------|
| **.NET SDK** | Latest LTS (10.0), update to next LTS when available |
| **NuGet Packages** | Lock to major version, update minor/patch regularly |
| **Docker Base Images** | Pin to specific tag (e.g., `10.0.0-alpine`), update quarterly |
| **Azure Services** | Use stable tiers, upgrade during maintenance windows |

**Dependency Update Process**:
- **Weekly**: Review and apply patch updates (security fixes)
- **Monthly**: Review and apply minor updates (new features, improvements)
- **Quarterly**: Major version updates (breaking changes require testing)

## Technology Decision Summary

| Category | Selected Technology | Alternative Considered | Decision Rationale |
|----------|---------------------|------------------------|-------------------|
| **Platform** | .NET 10 | .NET 8, Java 21 | Latest LTS, best performance, C# language features |
| **Web Framework** | ASP.NET Core Minimal APIs | ASP.NET Core MVC, FastEndpoints | Simplicity, performance, modern approach |
| **ORM** | Entity Framework Core | Dapper, NHibernate | Full-featured, LINQ, migrations, community support |
| **CQRS** | MediatR | Custom implementation | De facto standard, pipeline behaviors, easy testing |
| **Messaging** | Azure Service Bus | RabbitMQ, Azure Event Grid | Managed, reliable, deep Azure integration |
| **Authentication** | ASP.NET Core Identity + JWT | Azure AD B2C, IdentityServer | Built-in, sufficient for requirements, JWT for API |
| **UI (Admin)** | Blazor Server | Blazor WASM, React, Angular | .NET familiarity, full-stack C#, SignalR real-time |
| **Database** | Azure SQL Database | PostgreSQL, Cosmos DB | ACID, relational model, SQL Server compatibility |
| **Caching** | Azure Redis Cache | In-memory cache only | Distributed, session storage, scalable |
| **Hosting** | Azure Container Apps | Azure App Service, AKS | Serverless containers, auto-scale, cost-effective |
| **CI/CD** | GitHub Actions | Azure DevOps | GitHub integration, YAML workflows, community actions |

## Migration Technology Mapping

| COBOL/Mainframe | Modern .NET Equivalent | Notes |
|-----------------|------------------------|-------|
| **COBOL Program** | C# Class (Handler/Service) | Commands/Queries with MediatR |
| **Copybook** | C# Record/Class | DTOs, entities, value objects |
| **VSAM File** | Azure SQL Table | Relational database |
| **CICS Transaction** | REST API Endpoint | HTTP POST/GET/PUT/DELETE |
| **BMS Screen** | Blazor Component/Page | Web-based UI |
| **JCL Job** | Azure Function (Timer Trigger) | Scheduled batch processing |
| **CALL Statement** | Method Call / MediatR Send | Synchronous invocation |
| **MQ Message** | Service Bus Message | Asynchronous messaging |
| **DB2 SQL** | EF Core LINQ / SQL | Object-relational mapping |
| **PIC 9(11) COMP-3** | `decimal` (C#) | Packed decimal → .NET decimal |
| **PIC X(50)** | `string` (C#) | Character string |
| **PIC S9(9)V99 COMP-3** | `decimal(11,2)` | Monetary amount |

## Rationale for Key Technology Choices

### Why Modular Monolith over Microservices?
- **Simplicity**: Single deployment unit, simpler testing and debugging
- **Transaction Management**: ACID transactions across modules without distributed transactions
- **Operational Overhead**: No service mesh, no inter-service communication complexity
- **Team Size**: Appropriate for small-to-medium team
- **Future-Proof**: Clear module boundaries enable extraction to microservices later if needed

### Why Azure Container Apps over Azure App Service?
- **Containers**: Consistent dev/prod environments, Docker ecosystem
- **Scaling**: Serverless auto-scaling without managing infrastructure
- **Cost**: Pay for actual usage, scale to zero capability (not used initially but available)
- **Modern**: Cloud-native approach aligned with industry trends

### Why EF Core over Dapper?
- **Productivity**: Less boilerplate, LINQ queries, automatic change tracking
- **Migrations**: Version-controlled schema evolution
- **Modeling**: Rich entity relationships, lazy loading, eager loading
- **Performance**: Query splitting, compiled queries, batching (sufficient for CardDemo's load)
- **Trade-off**: Slightly slower than Dapper for raw queries, but acceptable for CardDemo's requirements

### Why Single Database over Database-per-Service?
- **ACID Transactions**: Maintain data consistency without distributed transactions
- **Simplicity**: Single connection string, single backup/restore
- **Cost**: One database instance vs. multiple
- **Data Integrity**: Foreign keys, constraints enforced at database level
- **Migration**: Easier migration from VSAM (single target)

## Technology Roadmap

### Phase 1 (Weeks 1-6): Foundation
- .NET 10 SDK installation
- Visual Studio 2022 setup
- Azure subscription and resources
- GitHub repository and CI/CD pipelines
- Base project structure (Clean Architecture template)

### Phase 2 (Weeks 7-16): Core Development
- ASP.NET Core Web API implementation
- EF Core with Azure SQL Database
- MediatR for CQRS
- ASP.NET Core Identity for authentication
- Unit and integration testing setup

### Phase 3 (Weeks 17-24): Advanced Features
- Azure Service Bus integration
- Azure Redis caching
- Application Insights observability
- Blazor Server admin portal
- Architecture testing

### Phase 4 (Weeks 25-30): Production Readiness
- Performance testing and optimization
- Security hardening (penetration testing, vulnerability scanning)
- Comprehensive E2E testing
- Documentation completion
- Production deployment and monitoring

## Conclusion

This technology stack represents a modern, maintainable, and scalable foundation for the CardDemo application. The selections balance:

- **Productivity**: Leverage .NET ecosystem for rapid development
- **Performance**: High-performance runtime, caching, optimized queries
- **Maintainability**: Clean Architecture, DDD, testability
- **Scalability**: Cloud-native services, auto-scaling, caching
- **Cost**: Pragmatic choices (modular monolith, single database, standard tiers)

All technologies are production-ready, actively maintained, and backed by strong communities and vendor support.

---

**Related Documents**:
- Architecture Overview: `overview.md`
- Solution Structure: `solution-structure.md`
- ADR-001: Use Modular Monolith over Microservices
- ADR-004: Azure Container Apps for Application Hosting
