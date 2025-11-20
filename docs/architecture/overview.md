# Architecture Overview: CardDemo Modernization

**Document Version**: 1.0  
**Last Updated**: 2025-11-20  
**Status**: Draft  
**Owner**: Software Architect

## Executive Summary

This document defines the target architecture for modernizing the AWS CardDemo mainframe application from COBOL/CICS/VSAM to a modern, cloud-native .NET application. The architecture leverages Clean Architecture principles, Domain-Driven Design, and CQRS patterns to create a maintainable, scalable, and testable system deployed on Microsoft Azure.

The modernization follows a **Modular Monolith** approach initially, with clear service boundaries that enable future decomposition to microservices if needed. This pragmatic approach balances the benefits of modern architecture patterns with the complexity appropriate for CardDemo's scale and business requirements.

## Architectural Style

**Pattern**: Modular Monolith with Microservices-Ready Design

**Rationale**: 
- CardDemo is a mid-sized application (7 functional modules, ~30 COBOL programs)
- Modular monolith reduces operational complexity while providing clear boundaries
- Clean Architecture with DDD enables future microservices extraction if needed
- Simplifies initial deployment, testing, and transaction management
- Azure Container Apps provides easy scaling without microservices overhead
- Team can focus on business logic modernization rather than distributed systems complexity

**Migration Path**: Start as modular monolith, extract high-value/high-load modules to separate services if business needs dictate (e.g., Transaction Processing for high throughput).

## Key Architectural Principles

### 1. Clean Architecture with Domain-Driven Design
**Description**: Application organized in concentric layers with dependencies flowing inward toward the domain core.

**Application**:
- **Domain Layer**: Pure business logic, entities, value objects, domain events (no infrastructure dependencies)
- **Application Layer**: Use cases, commands, queries, orchestration (CQRS with MediatR)
- **Infrastructure Layer**: Data access, external services, cross-cutting concerns
- **Presentation Layer**: Web API controllers, Blazor pages, API contracts

**Benefits**: Testability, maintainability, technology independence, clear separation of concerns

### 2. CQRS (Command Query Responsibility Segregation)
**Description**: Separate read and write operations with dedicated models and handlers.

**Application**:
- Commands modify state (CreateAccountCommand, UpdateCardCommand)
- Queries read data (GetAccountQuery, ListTransactionsQuery)
- MediatR mediates between controllers and handlers
- Read models optimized for queries (DTOs, ViewModels)
- Write models enforce business rules (Entities, Aggregates)

**Benefits**: Optimized performance, scalability, clear intent, simplified testing

### 3. Event-Driven Architecture (Asynchronous Processing)
**Description**: Domain events trigger asynchronous workflows and maintain consistency.

**Application**:
- Domain events published for significant business actions (AccountCreatedEvent, TransactionPostedEvent)
- Azure Service Bus for reliable event distribution
- Event handlers process background tasks (interest calculation, notifications, audit logging)
- Saga pattern for complex multi-step workflows

**Benefits**: Decoupling, scalability, resilience, eventual consistency support

### 4. API-First Design
**Description**: RESTful APIs as primary interface with OpenAPI specification.

**Application**:
- All functionality exposed via REST APIs
- OpenAPI/Swagger documentation auto-generated
- Blazor Server UI consumes internal APIs
- External integrations use same APIs
- API Gateway (Azure API Management) for unified entry point

**Benefits**: Consistency, testability, integration flexibility, clear contracts

### 5. Security by Design
**Description**: Security integrated at every layer, not bolted on.

**Application**:
- Authentication: ASP.NET Core Identity with JWT tokens
- Authorization: Policy-based + Claims-based (role and attribute-based)
- Data Protection: TLS 1.3 in transit, encryption at rest (Azure SQL TDE)
- Secrets Management: Azure Key Vault for all sensitive configuration
- API Security: Rate limiting, CORS policies, API keys for integration
- Audit Logging: Comprehensive tracking of all data access and modifications

**Benefits**: Defense in depth, compliance readiness, reduced attack surface

### 6. Cloud-Native Design
**Description**: Leverage Azure platform services for reliability and scalability.

**Application**:
- Azure Container Apps for application hosting (auto-scale, managed infrastructure)
- Azure SQL Database for data persistence (HA, automated backups)
- Azure Service Bus for messaging (reliable, durable queuing)
- Azure Key Vault for secrets (centralized, audited secret management)
- Azure Application Insights for observability (logging, metrics, tracing)
- Azure API Management for API gateway (rate limiting, transformation, routing)

**Benefits**: Reduced operational burden, built-in HA/DR, pay-as-you-grow

### 7. Test-Driven Quality
**Description**: Comprehensive automated testing at all levels.

**Application**:
- Unit Tests: All domain logic, handlers, validators (>80% coverage target)
- Integration Tests: API endpoints, database operations
- Architecture Tests: Enforce architectural rules (ArchUnit.NET)
- Contract Tests: API contract validation
- E2E Tests: Critical user journeys

**Benefits**: Confidence in changes, regression prevention, living documentation

## High-Level Architecture Diagram

```
┌───────────────────────────────────────────────────────────────────────┐
│                          CLIENT LAYER                                  │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────────┐  │
│  │  Blazor Server  │  │  Mobile Apps     │  │  External Systems  │  │
│  │  (Admin Portal) │  │  (Future)        │  │  (Integration)     │  │
│  └────────┬────────┘  └────────┬─────────┘  └──────────┬─────────┘  │
└───────────┼────────────────────┼───────────────────────┼─────────────┘
            │                    │                       │
            └────────────────────┼───────────────────────┘
                                 │
┌────────────────────────────────▼───────────────────────────────────────┐
│                      API GATEWAY (Azure API Management)                │
│  - Authentication/Authorization  - Rate Limiting  - Response Caching   │
│  - Request Routing               - Transformation - API Versioning     │
└────────────────────────────────┬───────────────────────────────────────┘
                                 │
┌────────────────────────────────▼───────────────────────────────────────┐
│                    PRESENTATION LAYER (ASP.NET Core)                   │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │                      CardDemo.WebAPI                             │  │
│  │  - Controllers (REST Endpoints)    - Middleware (Auth, Logging)  │  │
│  │  - DTOs / API Contracts            - Exception Handling          │  │
│  └──────────────────────────┬──────────────────────────────────────┘  │
└───────────────────────────────┼──────────────────────────────────────┘
                                │
┌───────────────────────────────▼───────────────────────────────────────┐
│                      APPLICATION LAYER (CQRS)                          │
│  ┌──────────────────────────────┐  ┌──────────────────────────────┐  │
│  │       COMMANDS (Writes)      │  │       QUERIES (Reads)        │  │
│  │  - CreateAccountCommand      │  │  - GetAccountQuery           │  │
│  │  - PostTransactionCommand    │  │  - ListTransactionsQuery     │  │
│  │  - UpdateCardCommand         │  │  - GetCardDetailsQuery       │  │
│  │  + Command Handlers          │  │  + Query Handlers            │  │
│  │  + Validators (FluentValid.) │  │  + Read Models (DTOs)        │  │
│  └──────────────┬───────────────┘  └───────────────┬──────────────┘  │
│                 │                                   │                  │
│                 │        ┌──────────────────┐     │                  │
│                 └───────►│  MediatR Router  │◄────┘                  │
│                          └─────────┬────────┘                         │
└────────────────────────────────────┼──────────────────────────────────┘
                                     │
┌────────────────────────────────────▼──────────────────────────────────┐
│                         DOMAIN LAYER (Business Logic)                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
│  │  AGGREGATES  │  │ DOMAIN EVENTS│  │ DOMAIN       │               │
│  │  - Account   │  │ - Account    │  │ SERVICES     │               │
│  │  - Card      │  │   Created    │  │ - Interest   │               │
│  │  - Customer  │  │ - Transaction│  │   Calculator │               │
│  │  - User      │  │   Posted     │  │ - Fraud      │               │
│  └──────────────┘  └──────────────┘  │   Detector   │               │
│  ┌──────────────┐  ┌──────────────┐  └──────────────┘               │
│  │ VALUE OBJECTS│  │  ENTITIES    │                                  │
│  │ - AccountId  │  │ - Transaction│  ┌──────────────┐               │
│  │ - CardNumber │  │ - Category   │  │ REPOSITORY   │               │
│  │ - Money      │  │ - Statement  │  │ INTERFACES   │               │
│  └──────────────┘  └──────────────┘  └──────────────┘               │
└───────────────────────────────────────────────────────────────────────┘
                                     │
┌────────────────────────────────────▼──────────────────────────────────┐
│                    INFRASTRUCTURE LAYER (Technical)                    │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │               DATA PERSISTENCE (EF Core)                          │ │
│  │  - CardDemoDbContext    - Repository Implementations             │ │
│  │  - Entity Configurations - Migrations                            │ │
│  │  - Azure SQL Database   - Connection Pooling                     │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │               MESSAGING (Azure Service Bus)                       │ │
│  │  - Event Publishers     - Event Subscribers                      │ │
│  │  - Message Serialization - Retry Policies                        │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │               EXTERNAL SERVICES                                   │ │
│  │  - Identity Service (ASP.NET Identity) - Logging (Serilog)       │ │
│  │  - Caching (Redis)                     - Email Service           │ │
│  └──────────────────────────────────────────────────────────────────┘ │
└───────────────────────────────────────────────────────────────────────┘
                                     │
┌────────────────────────────────────▼──────────────────────────────────┐
│                      CROSS-CUTTING CONCERNS                            │
│  - Application Insights (Monitoring)    - Azure Key Vault (Secrets)   │
│  - Polly (Resilience)                   - Serilog (Logging)           │
│  - Health Checks                        - Distributed Tracing         │
└───────────────────────────────────────────────────────────────────────┘
```

## Module Structure (Bounded Contexts)

The application is organized into **seven bounded contexts** (modules) with clear responsibilities:

### 1. Authentication Module (MOD-001)
**Responsibility**: User authentication, authorization, session management  
**Domain Entities**: User, Role, Session  
**Key Operations**: Login, Logout, Token Refresh, Password Management  
**Technology**: ASP.NET Core Identity, JWT tokens

### 2. Account Management Module (MOD-002)
**Responsibility**: Account CRUD, balance management, interest calculation  
**Domain Entities**: Account, Customer, AccountBalance  
**Key Operations**: View Account, Update Account, Calculate Interest, Export Data  
**Integration**: Card Module (account-card relationship), Transaction Module (balance updates)

### 3. Card Management Module (MOD-003)
**Responsibility**: Card lifecycle, card-account association  
**Domain Entities**: Card, CardStatus  
**Key Operations**: Search Cards, View Card Details, Update Card, Activate/Deactivate  
**Integration**: Account Module (linked to account)

### 4. Transaction Processing Module (MOD-004)
**Responsibility**: Transaction posting, transaction history, category management  
**Domain Entities**: Transaction, TransactionCategory, CategoryBalance  
**Key Operations**: Post Transaction, List Transactions, View Transaction Details, Daily Batch Posting  
**Integration**: Account Module (update balances), Card Module (card transactions)

### 5. User Management Module (MOD-005)
**Responsibility**: User administration (CRUD operations on system users)  
**Domain Entities**: SystemUser, UserProfile  
**Key Operations**: List Users, Add User, Update User, Delete User  
**Integration**: Authentication Module (user credentials)

### 6. Report Generation Module (MOD-006)
**Responsibility**: Statement generation, transaction reports, billing  
**Domain Entities**: Statement, Report, BillPayment  
**Key Operations**: Generate Statement, Transaction Report, Bill Payment Processing  
**Integration**: Account Module (statement data), Transaction Module (transaction data)

### 7. Batch Processing Module (MOD-007)
**Responsibility**: Scheduled jobs, data maintenance, backups  
**Domain Entities**: BatchJob, ScheduledTask  
**Key Operations**: Daily Transaction Posting, Monthly Interest Calculation, Cycle Resets  
**Integration**: All modules (orchestrates batch operations)

## Deployment Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          AZURE CLOUD PLATFORM                            │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                  Azure API Management (API Gateway)                 │ │
│  │  - Public IP: api.carddemo.com                                     │ │
│  │  - Rate Limiting, Caching, Routing                                 │ │
│  └────────────────────────────┬───────────────────────────────────────┘ │
│                                │                                          │
│  ┌────────────────────────────▼───────────────────────────────────────┐ │
│  │              Azure Container Apps Environment                       │ │
│  │  ┌──────────────────────────────────────────────────────────────┐  │ │
│  │  │  CardDemo.WebAPI Container                                    │  │ │
│  │  │  - Auto-scale: 1-10 instances                                │  │ │
│  │  │  - CPU/Memory triggers                                       │  │ │
│  │  │  - Health checks enabled                                     │  │ │
│  │  └──────────────────────────────────────────────────────────────┘  │ │
│  │  ┌──────────────────────────────────────────────────────────────┐  │ │
│  │  │  CardDemo.BatchProcessor Container (scheduled)               │  │ │
│  │  │  - Cron schedule: Daily at 2 AM, Monthly on 1st             │  │ │
│  │  │  - Single instance (no scale)                               │  │ │
│  │  └──────────────────────────────────────────────────────────────┘  │ │
│  └─────────────────────────────┬──────────────────────────────────────┘ │
│                                 │                                         │
│  ┌─────────────────────────────┼─────────────────────────────────────┐  │
│  │  Azure SQL Database         │  Azure Service Bus                  │  │
│  │  - DTU: S3 (100 DTU)        │  - Namespace: carddemo-bus         │  │
│  │  - Storage: 250 GB          │  - Topics: account-events,         │  │
│  │  - Geo-replication: Enabled │    transaction-events              │  │
│  │  - TDE: Enabled             │  - Subscriptions per consumer      │  │
│  └─────────────────────────────┼─────────────────────────────────────┘  │
│                                 │                                         │
│  ┌─────────────────────────────┼─────────────────────────────────────┐  │
│  │  Azure Redis Cache          │  Azure Key Vault                    │  │
│  │  - Tier: Standard           │  - Secrets: DB conn, API keys       │  │
│  │  - Size: 1 GB               │  - Access: Managed Identity only    │  │
│  │  - TTL: 5 min               │  - Audit logs enabled               │  │
│  └─────────────────────────────┼─────────────────────────────────────┘  │
│                                 │                                         │
│  ┌─────────────────────────────▼─────────────────────────────────────┐  │
│  │  Azure Application Insights + Log Analytics                        │  │
│  │  - Distributed tracing       - Custom metrics                     │  │
│  │  - Log aggregation           - Alert rules                        │  │
│  └────────────────────────────────────────────────────────────────────┘  │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │  Azure DevOps / GitHub Actions (CI/CD)                             │ │
│  │  - Build pipeline           - Release pipeline                     │ │
│  │  - Automated testing        - Infrastructure as Code (Terraform)   │ │
│  └────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
```

## Data Architecture

### Database Strategy: Single Database (Initial Phase)

**Decision**: Start with single Azure SQL Database with schema-per-module organization.

**Rationale**:
- Simplifies transaction management (ACID guarantees)
- Reduces operational complexity
- Appropriate for CardDemo's data volume and complexity
- Easy migration from VSAM files
- Can refactor to database-per-service later if needed

**Schema Organization**:
- `auth` schema: User, Role, Session tables
- `account` schema: Account, Customer, AccountBalance tables
- `card` schema: Card, CardStatus tables
- `transaction` schema: Transaction, TransactionCategory, CategoryBalance tables
- `report` schema: Statement, Report tables
- `audit` schema: AuditLog, EventLog tables

### Data Migration Strategy

**Phase 1: Extract** (VSAM → CSV)
- Export VSAM files to delimited text format
- Preserve all data, including dates and numeric fields
- Validate record counts and checksums

**Phase 2: Transform** (CSV → SQL)
- Convert COBOL data types to SQL types (COMP-3 → DECIMAL, PIC X → NVARCHAR)
- Parse dates (YYYYMMDD → DATE)
- Create surrogate keys (GUIDs) while preserving natural keys
- Hash passwords (COBOL plaintext → bcrypt hashes)

**Phase 3: Load** (SQL Import)
- Bulk insert into Azure SQL Database
- Apply referential integrity constraints
- Create indexes for performance
- Validate data integrity (row counts, foreign keys, business rule checks)

### Entity Relationship Overview

```
┌──────────────┐         ┌──────────────┐         ┌──────────────┐
│   Customer   │◄───────►│   Account    │◄───────►│     Card     │
│              │ 1     * │              │ 1     * │              │
│ - CustomerId │         │ - AccountId  │         │ - CardId     │
│ - FirstName  │         │ - CustId (FK)│         │ - AcctId(FK) │
│ - LastName   │         │ - Balance    │         │ - CardNumber │
│ - SSN        │         │ - CreditLimit│         │ - Status     │
└──────────────┘         └───────┬──────┘         └──────────────┘
                                 │
                                 │ 1
                                 │
                                 │ *
                         ┌───────▼──────┐
                         │ Transaction  │
                         │              │
                         │ - TransId    │
                         │ - AcctId(FK) │
                         │ - Amount     │
                         │ - Type       │
                         │ - Timestamp  │
                         └──────────────┘

┌──────────────┐         ┌──────────────┐
│     User     │◄───────►│     Role     │
│              │ *     * │              │
│ - UserId     │         │ - RoleId     │
│ - Username   │         │ - RoleName   │
│ - PasswordHash         │ - Permissions│
└──────────────┘         └──────────────┘
```

## Security Architecture

### Authentication Flow

```
1. User submits credentials → WebAPI
2. WebAPI validates credentials (ASP.NET Core Identity)
3. Generate JWT token (signed with secret from Key Vault)
4. Return token to client
5. Client includes token in Authorization header for subsequent requests
6. API Gateway validates token signature
7. WebAPI extracts claims (user ID, roles) from token
8. Apply authorization policies
```

### Authorization Model

**Role-Based Access Control (RBAC)**:
- **Administrator**: Full access to all modules including user management
- **Customer Service Rep**: Read/write access to accounts, cards, transactions
- **Auditor**: Read-only access to all data, full access to audit logs
- **Batch Processor**: Service account for automated jobs

**Claims-Based Authorization**:
- Custom claims: `department`, `region`, `customerId` (for customer self-service)
- Policy examples: `[Authorize(Policy = "RequireAdministratorRole")]`

### Data Protection

**In Transit**: TLS 1.3 for all HTTP traffic (enforced at API Gateway)  
**At Rest**: Azure SQL TDE (Transparent Data Encryption), Key Vault for secrets  
**Application Level**: Sensitive fields encrypted in application (SSN, card numbers) using Data Protection API

## Observability

### Three Pillars

**1. Logging** (Structured with Serilog)
- Application logs: Info, Warning, Error levels
- Audit logs: All data modifications with user context
- Performance logs: Slow queries, long-running operations
- Sink: Azure Application Insights + Azure Log Analytics

**2. Metrics** (Custom + Platform)
- Business metrics: Transactions per second, authentication success rate
- Technical metrics: Response times (p50, p95, p99), error rates, throughput
- Infrastructure metrics: CPU, memory, disk I/O, network
- Dashboards: Azure Monitor dashboards, Grafana (optional)

**3. Tracing** (Distributed with OpenTelemetry)
- Request tracing across API Gateway → WebAPI → Database
- Correlation IDs for request tracking
- Dependency tracking (SQL, Service Bus, Redis)
- Trace viewer: Application Insights Application Map

### Health Checks

- **/health/live**: Liveness probe (container is running)
- **/health/ready**: Readiness probe (can accept traffic - DB, Service Bus reachable)
- **/health/startup**: Startup probe (initialization complete)

## Non-Functional Requirements

### Performance
| Metric | Target | Measurement |
|--------|--------|-------------|
| API Response Time (p95) | < 200ms | Application Insights |
| Database Query Time (p95) | < 50ms | SQL Query Store |
| Transaction Throughput | 1000 TPS | Load testing |
| Page Load Time | < 2s | Browser metrics |

### Scalability
- **Horizontal Scaling**: Auto-scale Container Apps (1-10 instances) based on CPU (>70%) and memory (>80%)
- **Database Scaling**: Azure SQL DTU scaling (S3 → P1 if needed), read replicas for queries
- **Caching**: Redis for frequently accessed data (user sessions, account lookups)

### Reliability
- **Availability**: 99.9% uptime SLA (Azure Container Apps SLA)
- **Fault Tolerance**: Circuit breakers (Polly) for external dependencies
- **Retry Policies**: Exponential backoff for transient failures
- **Data Durability**: Automated backups (daily), geo-replication for DR

### Security
- **Authentication**: Multi-factor authentication (MFA) support
- **Authorization**: Fine-grained policies (RBAC + claims)
- **Data Protection**: Encryption in transit (TLS 1.3), at rest (TDE), application-level (Data Protection API)
- **Secrets**: Azure Key Vault (no secrets in code or config files)
- **Compliance**: PCI-DSS considerations (no card numbers in logs, audit trails)

## Migration Strategy (Strangler Fig Pattern)

### Overview
Gradually replace COBOL/CICS functionality with .NET services while maintaining operational mainframe during transition.

### Phase 1: Foundation (Weeks 1-6)
**Goal**: Establish infrastructure and authentication

**Deliverables**:
- Azure environment setup (Terraform scripts)
- CI/CD pipelines (GitHub Actions)
- Authentication service (MOD-001) deployed
- Data migration tool for user data
- Admin portal for user management

**Success Criteria**: Admin users can authenticate and manage system users via modern UI

### Phase 2: Core Services (Weeks 7-16)
**Goal**: Implement account and card management

**Deliverables**:
- Account Management module (MOD-002) - online operations
- Card Management module (MOD-003)
- Data migration for accounts, cards, customers
- Integration with mainframe (read-only) for transaction history
- Customer service portal

**Success Criteria**: CSRs can view/update accounts and cards via modern UI while transactions still processed on mainframe

### Phase 3: Transaction Processing (Weeks 17-24)
**Goal**: Migrate transaction processing

**Deliverables**:
- Transaction Processing module (MOD-004) - online and batch
- Two-phase commit for writes to both mainframe and modern system
- Data reconciliation tools
- Parallel run (mainframe and .NET both process transactions)

**Success Criteria**: Transactions processed by .NET system, validated against mainframe for 2 weeks with <0.01% discrepancy

### Phase 4: Reporting and Cutover (Weeks 25-30)
**Goal**: Complete migration and decommission mainframe

**Deliverables**:
- Report Generation module (MOD-006)
- Batch Processing module (MOD-007) - all scheduled jobs
- Full data migration (historical transactions)
- User acceptance testing (UAT)
- Cutover plan and rollback procedures

**Success Criteria**: All functionality migrated, mainframe decommissioned

## Architecture Decision Records (ADRs)

Key architectural decisions documented in separate ADR files:

- **ADR-001**: Use Modular Monolith over Microservices
- **ADR-002**: Single Database with Schema-per-Module
- **ADR-003**: CQRS with MediatR for Application Layer
- **ADR-004**: Azure Container Apps for Application Hosting
- **ADR-005**: ASP.NET Core Identity with JWT for Authentication
- **ADR-006**: Azure Service Bus for Asynchronous Messaging
- **ADR-007**: Entity Framework Core for Data Access
- **ADR-008**: Clean Architecture with DDD Tactical Patterns
- **ADR-009**: Blazor Server for Admin Portal
- **ADR-010**: Strangler Fig Pattern for Migration

See `/docs/architecture/adrs/` for detailed ADR documents.

## Design Patterns

Key patterns documented in separate pattern files:

- **PATTERN-001**: CQRS Implementation with MediatR
- **PATTERN-002**: Repository Pattern with EF Core
- **PATTERN-003**: Domain Events and Event Handlers
- **PATTERN-004**: Aggregate Root Design
- **PATTERN-005**: Value Objects
- **PATTERN-006**: Saga Pattern for Distributed Transactions
- **PATTERN-007**: API Gateway Pattern

See `/docs/architecture/patterns/` for detailed pattern documentation.

## Development Guidelines

See separate guideline documents:

- **Coding Standards**: C# conventions, naming, formatting
- **Security Guidelines**: Authentication, authorization, data protection
- **Testing Standards**: Unit, integration, architecture tests
- **API Design Guidelines**: RESTful conventions, versioning, error handling
- **Database Guidelines**: EF Core migrations, query optimization

See `/docs/architecture/guidelines/` for detailed guidelines.

## Conclusion

This architecture provides a modern, maintainable, and scalable foundation for the CardDemo application. The modular monolith approach balances pragmatism with modern architectural principles, while Clean Architecture and DDD patterns ensure long-term maintainability and testability.

The architecture is designed for the current scale of CardDemo while providing clear paths for future growth (microservices extraction, additional channels, cloud-native features). The Strangler Fig migration pattern minimizes risk by enabling incremental replacement of mainframe functionality.

## Next Steps

1. Review and approve ADRs (see `/docs/architecture/adrs/`)
2. Detailed design of each module (see `/docs/implementation/components/`)
3. Setup Azure environment (Terraform scripts)
4. Implement Authentication module (MOD-001) as proof of concept
5. Data migration tool development
6. Begin developer onboarding with coding guidelines

---

**Related Documents**:
- Technology Stack: `technology-stack.md`
- Solution Structure: `solution-structure.md`
- Business Requirements: `/docs/analysis/architecture/business-requirements/`
- Component Designs: `/docs/implementation/components/`
