# Architecture Documentation

This directory contains the complete architecture definition for the modernized CardDemo application.

**Status**: ✅ **COMPLETE**  
**Last Updated**: 2025-11-20  
**Owner**: Software Architect

## Overview

The CardDemo modernization adopts a **Modular Monolith** architecture with Clean Architecture principles, Domain-Driven Design, and CQRS patterns. The application will be deployed on **Microsoft Azure** using **.NET 10**.

## Core Architecture Documents

### 1. [Architecture Overview](overview.md)
**Status**: ✅ Complete  
**Pages**: 25+ pages

Comprehensive architecture overview including:
- Executive summary and architectural style
- Key architectural principles
- High-level architecture diagram
- Module structure (7 bounded contexts)
- Deployment architecture
- Data architecture
- Security architecture
- Observability strategy
- Non-functional requirements
- Migration strategy (Strangler Fig pattern)
- Architecture Decision Records (ADR) summary

**Read this first** to understand the overall architecture.

### 2. [Technology Stack](technology-stack.md)
**Status**: ✅ Complete  
**Pages**: 20+ pages

Complete technology selections with rationale:
- Core platform (.NET 10, C# 14, ASP.NET Core)
- Application framework (MediatR, FluentValidation, AutoMapper, Polly)
- Data access (Entity Framework Core, Azure SQL Database)
- Caching (Azure Redis Cache)
- Messaging (Azure Service Bus)
- Security (ASP.NET Core Identity, JWT, Azure Key Vault)
- Cloud services (Azure Container Apps, API Management, Application Insights)
- Development tools (Visual Studio, Git, GitHub Actions, Docker)
- Testing frameworks (xUnit, Moq, FluentAssertions, Testcontainers)
- Code quality tools (SonarQube, StyleCop, ArchUnit.NET)

**Use this** when making technology decisions or setting up development environment.

### 3. [Solution Structure](solution-structure.md)
**Status**: ✅ Complete  
**Pages**: 30+ pages

Detailed solution organization:
- Root directory structure
- Core projects (Domain, Application)
- Infrastructure projects (Persistence, Messaging)
- Presentation projects (WebAPI, Blazor Admin Portal)
- Shared projects (Kernel, Contracts)
- Test projects (Unit, Integration, Architecture, E2E)
- File naming conventions
- Namespace conventions
- Configuration management
- Build configuration (Directory.Build.props, Directory.Packages.props)
- Docker configuration

**Use this** when creating new projects or files.

## Architecture Decision Records (ADRs)

ADRs document major architecture decisions with context, alternatives considered, and consequences.

| ID | Title | Date | Status | Impact |
|----|-------|------|--------|--------|
| [ADR-001](adrs/ADR-001-use-modular-monolith-architecture.md) | Use Modular Monolith over Microservices | 2025-11-20 | ✅ Accepted | **High** - Defines overall architecture |
| [ADR-003](adrs/ADR-003-cqrs-with-mediatr.md) | CQRS with MediatR for Application Layer | 2025-11-20 | ✅ Accepted | **High** - Defines application layer structure |

**Pending ADRs** (to be created as needed):
- ADR-002: Single Database with Schema-per-Module
- ADR-004: Azure Container Apps for Application Hosting
- ADR-005: ASP.NET Core Identity with JWT for Authentication
- ADR-006: Azure Service Bus for Asynchronous Messaging
- ADR-007: Entity Framework Core for Data Access
- ADR-008: Clean Architecture with DDD Tactical Patterns
- ADR-009: Blazor Server for Admin Portal
- ADR-010: Strangler Fig Pattern for Migration

## Design Patterns

Detailed implementation guides for key design patterns.

| ID | Pattern | Category | Status |
|----|---------|----------|--------|
| [PATTERN-001](patterns/PATTERN-001-cqrs-implementation.md) | CQRS Implementation with MediatR | Application Layer | ✅ Complete |

**Pending Patterns** (to be created as needed):
- PATTERN-002: Repository Pattern with EF Core
- PATTERN-003: Domain Events and Event Handlers
- PATTERN-004: Aggregate Root Design
- PATTERN-005: Value Objects
- PATTERN-006: Saga Pattern for Distributed Transactions
- PATTERN-007: API Gateway Pattern

## Development Guidelines

**Status**: To be created

Planned guidelines:
- `guidelines/coding-standards.md` - C# conventions, naming, formatting
- `guidelines/security-guidelines.md` - Authentication, authorization, data protection
- `guidelines/testing-standards.md` - Unit, integration, architecture tests
- `guidelines/api-design-guidelines.md` - RESTful conventions, versioning, error handling
- `guidelines/database-guidelines.md` - EF Core migrations, query optimization

## Key Architecture Decisions Summary

### Architectural Style
- **Pattern**: Modular Monolith (microservices-ready)
- **Rationale**: Balances simplicity with modern architecture; can extract to microservices later if needed
- **Impact**: Single deployment, shared database, clear module boundaries

### Technology Platform
- **.NET 10 (LTS)**: Latest long-term support release, best performance
- **C# 14**: Modern language features (records, pattern matching, nullable reference types)
- **Azure Cloud**: First-party services for reliability and integration

### Application Architecture
- **Clean Architecture**: Domain → Application → Infrastructure → Presentation
- **Domain-Driven Design**: Aggregates, entities, value objects, domain events
- **CQRS**: Command/query separation with MediatR
- **Event-Driven**: Azure Service Bus for asynchronous workflows

### Data Architecture
- **Single Database**: Azure SQL Database (initially)
- **Schema-per-Module**: Logical separation, prepare for future database separation
- **Entity Framework Core**: ORM for data access
- **Repository Pattern**: Abstract data access behind interfaces

### Deployment Architecture
- **Azure Container Apps**: Serverless containers, auto-scaling
- **Azure API Management**: API gateway, rate limiting, caching
- **Docker**: Containerization for consistency

### Migration Strategy
- **Strangler Fig Pattern**: Gradually replace COBOL functionality
- **Phased Approach**: Foundation → Core Services → Advanced Features → Cutover
- **Data Migration**: ETL from VSAM to Azure SQL

## Module Overview

Seven bounded contexts (modules) with clear responsibilities:

1. **MOD-001: Authentication** - User authentication, authorization, session management
2. **MOD-002: Account Management** - Account CRUD, balance management, interest calculation
3. **MOD-003: Card Management** - Card lifecycle, card-account association
4. **MOD-004: Transaction Processing** - Transaction posting, history, category management
5. **MOD-005: User Management** - User administration (CRUD operations)
6. **MOD-006: Report Generation** - Statements, transaction reports, billing
7. **MOD-007: Batch Processing** - Scheduled jobs, data maintenance

## Architecture Diagrams

### High-Level Architecture
```
┌─────────────────────────────────────────────────┐
│              Client Layer                        │
│  (Blazor, Mobile, External Systems)             │
└──────────────────┬──────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────┐
│         API Gateway (Azure APIM)                 │
└──────────────────┬──────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────┐
│      Presentation (ASP.NET Core WebAPI)          │
└──────────────────┬──────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────┐
│      Application (CQRS with MediatR)             │
│  Commands / Queries / Handlers / Validators     │
└──────────────────┬──────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────┐
│         Domain (Business Logic)                  │
│  Aggregates / Entities / Value Objects / Events │
└──────────────────┬──────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────┐
│      Infrastructure (Technical Concerns)         │
│  EF Core / Azure SQL / Service Bus / Redis      │
└─────────────────────────────────────────────────┘
```

See [overview.md](overview.md) for detailed diagrams.

## Non-Functional Requirements

| Category | Target | Measurement |
|----------|--------|-------------|
| **Performance** | API p95 < 200ms | Application Insights |
| **Scalability** | 1000 TPS | Load testing |
| **Availability** | 99.9% uptime | Azure Monitor |
| **Security** | PCI-DSS considerations | Security audit |

## Migration Timeline

- **Phase 1** (Weeks 1-6): Foundation - Authentication, infrastructure setup
- **Phase 2** (Weeks 7-16): Core Services - Account, Card, Transaction (online)
- **Phase 3** (Weeks 17-24): Advanced Features - Transaction (batch), User, Reports
- **Phase 4** (Weeks 25-30): Migration & Cutover - Data migration, parallel run, mainframe decommission

## Next Steps

1. ✅ ~~Architecture Definition~~ (COMPLETE)
2. **Continue Business Requirements** for remaining modules (MOD-004 through MOD-007)
3. **Begin Detailed Specification** for completed modules (MOD-001, MOD-002, MOD-003)
4. **Create Development Guidelines** (coding standards, security, testing)
5. **Proof of Concept**: Implement MOD-001 (Authentication) to validate architecture
6. **Setup Azure Environment**: Create resources per architecture specification
7. **CI/CD Pipeline**: Implement GitHub Actions workflows

## Related Documentation

- **Business Requirements**: `/docs/analysis/architecture/business-requirements/`
- **COBOL Analysis**: `/docs/analysis/cobol/`
- **Component Status**: `/docs/state/component-status.md`
- **Decision Log**: `/docs/state/decision-log.md`
- **Implementation**: `/docs/implementation/` (to be created during development)

## Questions or Feedback?

For questions about the architecture or to propose changes:
1. Review existing ADRs to understand decisions
2. If proposing a change, create a new ADR with alternatives and rationale
3. Discuss with Software Architect and team before implementing

## Architecture Principles to Remember

1. **Dependencies flow inward** (Presentation → Infrastructure → Application → Domain)
2. **Domain layer has no dependencies** (pure business logic)
3. **Modules communicate via interfaces** (in-process) or events (asynchronous)
4. **Commands modify state, Queries read data** (CQRS)
5. **Use Result<T> for error handling** (not exceptions for expected failures)
6. **Test at all layers** (unit, integration, architecture, E2E)
7. **Security by design** (not bolted on)
8. **Cloud-native patterns** (leverage Azure services)

---

**Architecture Status**: ✅ **COMPLETE - READY FOR IMPLEMENTATION**

The architecture is production-ready and provides a solid foundation for the CardDemo modernization. All major decisions have been documented, and the structure supports the business requirements identified so far.
