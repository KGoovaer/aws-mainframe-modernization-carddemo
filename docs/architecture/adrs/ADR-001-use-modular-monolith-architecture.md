# ADR-001: Use Modular Monolith over Microservices Architecture

**Status**: Accepted  
**Date**: 2025-11-20  
**Deciders**: Software Architect, Technical Lead  
**Consulted**: Application Architect, Development Team

## Context

The CardDemo mainframe application consists of approximately 30 COBOL programs organized into 7 functional modules (Authentication, Account Management, Card Management, Transaction Processing, User Management, Reports, Batch Processing). We need to decide on the architectural style for the modernized .NET application: microservices, modular monolith, or traditional monolith.

### Key Considerations

1. **Application Scale**: Mid-sized application with ~30 programs, 7 bounded contexts
2. **Team Size**: Small-to-medium development team (likely 3-8 developers)
3. **Deployment Frequency**: Moderate (weekly to bi-weekly releases expected)
4. **Data Relationships**: High coupling between accounts, cards, and transactions
5. **Transaction Requirements**: Strong consistency needed for financial operations
6. **Operational Maturity**: Team transitioning from mainframe, limited cloud-native experience initially
7. **Time to Market**: Pressure to deliver modernized system incrementally

### Options Considered

#### Option 1: Microservices Architecture
**Description**: Decompose application into 7+ independent services, each with its own database, deployed and scaled independently.

**Pros**:
- Independent deployability and scalability
- Technology heterogeneity (mix technologies per service)
- Fault isolation (one service failure doesn't crash entire app)
- Team autonomy (teams own services end-to-end)
- Aligned with cloud-native best practices

**Cons**:
- High operational complexity (service discovery, API gateway, distributed tracing)
- Distributed transaction challenges (eventual consistency, saga patterns)
- Network latency and reliability concerns
- More complex testing (integration and E2E tests)
- Requires mature DevOps practices and tooling
- Higher initial infrastructure costs
- Steep learning curve for team transitioning from mainframe
- Debugging and troubleshooting more difficult

#### Option 2: Modular Monolith
**Description**: Single deployable unit with clear internal boundaries between modules, using Clean Architecture and DDD patterns to enforce separation.

**Pros**:
- **Simplicity**: Single deployment artifact, easier to test and debug
- **Strong Consistency**: ACID transactions across modules without distributed transaction complexity
- **Performance**: In-process method calls (no network overhead)
- **Easier Development**: Simpler IDE navigation, refactoring across modules
- **Lower Operational Overhead**: One database, one deployment pipeline initially
- **Gradual Migration**: Can extract modules to microservices later if needed
- **Team Familiarity**: More similar to traditional application development
- **Cost-Effective**: Lower infrastructure costs (single database, single compute)

**Cons**:
- All modules deployed together (cannot deploy independently)
- Shared database requires schema coordination
- Risk of tight coupling if boundaries not enforced
- Single point of failure (entire app goes down together)
- Limited technology diversity (all modules use same tech stack)

#### Option 3: Traditional Monolith
**Description**: Layered architecture without enforced module boundaries, typical N-tier application.

**Pros**:
- Simplest initial development
- Team familiar with pattern

**Cons**:
- No clear boundaries (leads to "big ball of mud")
- Difficult to extract functionality later
- Poor maintainability over time
- Not future-proof for potential microservices evolution

## Decision

**We will adopt a Modular Monolith architecture** with the following characteristics:

1. **Clear Module Boundaries**: Seven modules (Authentication, Accounts, Cards, Transactions, Users, Reports, Batch) with well-defined interfaces
2. **Clean Architecture**: Strict dependency rules (Domain → Application → Infrastructure → Presentation)
3. **Domain-Driven Design**: Bounded contexts, aggregates, domain events
4. **CQRS with MediatR**: Command/query separation within modules
5. **Shared Database**: Single Azure SQL Database with schema-per-module organization
6. **Event-Driven Communication**: Domain events (in-process) and integration events (Azure Service Bus) for asynchronous workflows
7. **Microservices-Ready**: Design modules as if they were separate services (explicit APIs, no cross-module database queries)

### Architecture Enforcement

To prevent degradation into a traditional monolith, we will:

1. **Architecture Tests**: Use ArchUnit.NET to enforce dependency rules
2. **Code Reviews**: Verify module boundaries respected
3. **Module Interfaces**: All inter-module communication through interfaces (in-process) or events (asynchronous)
4. **Database Organization**: Schemas per module, no cross-schema foreign keys initially
5. **Documentation**: Clear module ownership and API contracts

## Rationale

### Why Not Microservices (Now)?

1. **Team Maturity**: Team is transitioning from mainframe to cloud-native; microservices add significant operational complexity
2. **Application Scale**: CardDemo's scale (~30 programs, moderate load) doesn't justify microservices overhead
3. **Data Coupling**: Strong relationships between accounts, cards, and transactions require careful distributed transaction handling
4. **Time Pressure**: Modular monolith faster to market; can refactor to microservices later if business justifies it
5. **Cost**: Single database and compute instance more cost-effective initially

### Why Not Traditional Monolith?

1. **Maintainability**: Clear module boundaries prevent "big ball of mud"
2. **Future-Proofing**: Can extract high-value modules (e.g., Transaction Processing) to microservices later
3. **Scalability**: Modular design enables horizontal scaling of entire app; if bottlenecks emerge, can extract specific modules
4. **Team Organization**: Modules map to team ownership even within monolith

### Why Modular Monolith is Right for CardDemo

1. **Pragmatic**: Balances modern architecture principles with appropriate complexity
2. **Risk Mitigation**: Simpler deployment and testing reduce migration risk
3. **Transactional Integrity**: ACID transactions across modules without saga complexity
4. **Performance**: In-process calls faster than remote API calls
5. **Developer Experience**: Easier debugging, refactoring, and testing
6. **Incremental Evolution**: Start simple, extract microservices only if business value justifies it

## Consequences

### Positive

- **Faster Time to Market**: Less infrastructure to build, simpler deployment
- **Lower Operational Complexity**: Single database, single deployment, simpler monitoring
- **Easier Testing**: Unit, integration, and E2E tests simpler without distributed system
- **Better Performance**: No network latency for inter-module communication
- **ACID Transactions**: Strong consistency across modules without compensation logic
- **Team Productivity**: Less cognitive overhead from distributed systems concerns
- **Cost Savings**: Lower infrastructure costs (single DB, single compute)

### Negative

- **Deployment Coupling**: All modules deployed together; cannot deploy independently
- **Scaling Granularity**: Scale entire application, not individual modules (initially)
- **Shared Database**: Schema changes require coordination across modules
- **Technology Lock-In**: All modules use .NET stack (acceptable for CardDemo)

### Mitigations

- **Enforce Boundaries**: Architecture tests prevent tight coupling
- **Event-Driven**: Use events for asynchronous workflows, enabling future extraction
- **Database Schemas**: Organize by module, no cross-schema queries, prepare for eventual database separation
- **Monitor Hotspots**: Use Application Insights to identify modules that might benefit from extraction
- **Document Interfaces**: Treat module interfaces as API contracts for future extraction

### Future Microservices Extraction

If business needs dictate (high load, team growth, independent deployment), we can extract modules to microservices:

**Candidates for Extraction** (in priority order):
1. **Transaction Processing**: High volume, could benefit from independent scaling
2. **Batch Processing**: Different scaling profile (scheduled jobs vs. online)
3. **Authentication**: Could use managed identity service (Azure AD B2C)

**Extraction Process**:
1. Refactor module to use explicit API contracts (already done in modular monolith)
2. Extract module database schema to separate database
3. Change in-process calls to HTTP calls (API Gateway)
4. Deploy module as separate Container App
5. Implement event-driven communication for asynchronous workflows

## Validation

We will validate this decision after 6 months of development by reviewing:

- **Development Velocity**: Are we shipping features faster than with microservices?
- **Bug Rate**: Are we seeing fewer production issues than expected with microservices?
- **Performance**: Are we meeting performance SLAs with monolithic deployment?
- **Team Satisfaction**: Is team productive and happy with architecture?
- **Module Coupling**: Have we maintained clear boundaries?

If negative trends emerge, we can re-evaluate and extract problematic modules.

## Related Decisions

- **ADR-002**: Single Database with Schema-per-Module
- **ADR-003**: CQRS with MediatR for Application Layer
- **ADR-004**: Azure Container Apps for Application Hosting
- **ADR-008**: Clean Architecture with DDD Tactical Patterns

## References

- [Modular Monoliths - Kamil Grzybek](https://www.kamilgrzybek.com/design/modular-monolith-primer/)
- [Microservices vs Monolith - Martin Fowler](https://martinfowler.com/articles/dont-start-monolith.html)
- [Monolith First - Martin Fowler](https://martinfowler.com/bliki/MonolithFirst.html)
- [.NET Microservices: Architecture for Containerized .NET Applications - Microsoft](https://learn.microsoft.com/en-us/dotnet/architecture/microservices/)
