# POC Architecture Documentation

This folder contains architecture documentation for the **Proof of Concept (POC)** implementation of the CardDemo modernization.

## Purpose

The POC architecture is designed for **rapid validation** of business logic translation from COBOL to Java Spring Boot with minimal infrastructure complexity.

**Key Goals**:
- ✅ Prove COBOL business logic can be correctly translated to Java
- ✅ Validate data model and entity relationships
- ✅ Enable quick stakeholder demonstrations
- ✅ Inform decisions for final production architecture
- ⚠️ **NOT** production-ready

## POC vs Final Architecture

📋 **See**: [Complete Decision Guide](DECISION-GUIDE.md) for choosing between POC and Final Architecture

| Aspect | POC | Final (Production) |
|--------|-----|-------------------|
| **Purpose** | Validation & learning | Production deployment |
| **Database** | H2 (local file) | Azure SQL Database |
| **Architecture** | 3-layer | Clean Architecture + DDD |
| **Patterns** | Repository, Service | CQRS, Event Sourcing, Repository |
| **Deployment** | Local (`dotnet run`) | Azure Container Apps |
| **Complexity** | Low | High |
| **Development Time** | 1-2 weeks | 2-3 months |

## Documentation Structure

```
poc/
├── overview.md                    # POC architecture overview and philosophy
├── technology-stack.md            # Simplified technology selections
├── solution-structure.md          # Single-project structure
└── patterns/
    ├── PATTERN-POC-001-repository-pattern.md     # Data access pattern
    └── PATTERN-POC-002-service-layer.md          # Business logic pattern
```

## Core Documents

### 1. [Overview](overview.md)
**Read this first** - Comprehensive POC architecture description including:
- Architecture philosophy and principles
- POC vs final architecture comparison
- System components and their interactions
- Data model overview
- API endpoints and UI pages
- Validation goals and success criteria

### 2. [Technology Stack](technology-stack.md)
Complete POC technology selections:
- Java Spring Boot 10 with Java 14
- H2 database with Spring Data JPA
- Angular Server for UI
- xUnit for testing
- No CQRS, no messaging, no cloud services

### 3. [Solution Structure](solution-structure.md)
Single-project structure for rapid development:
- `carddemo-poc/` - All application code
- `CardDemo.POC.Tests/` - Unit tests
- Folder-based organization (Controllers, Services, Data, etc.)

### 4. [Patterns](patterns/)
Design patterns used in POC:
- **PATTERN-POC-001**: Repository Pattern (data access abstraction)
- **PATTERN-POC-002**: Service Layer Pattern (business logic encapsulation)

## Quick Start

### Prerequisites
- Java Spring Boot 10 SDK installed
- Visual Studio 2025 or VS Code
- That's it! (no cloud account, no database server)

### Running the POC
```bash
cd src/poc/CardDemo.POC
dotnet restore
dotnet ef database update
dotnet run
```

Access at: https://localhost:5001

## Key Characteristics

### Simplicity First
- **Minimal setup**: < 5 minutes to get running
- **Local development**: No cloud dependencies
- **Single project**: All code in one place
- **File-based database**: H2 (no server installation)

### Validation Focus
- **Business logic correctness**: Match COBOL behavior exactly
- **Data model validity**: Prove entity relationships work
- **User experience**: Verify UI flows match expectations
- **Feasibility**: Confirm COBOL can be translated

### NOT Production-Ready
- ⚠️ No scalability (single instance, H2)
- ⚠️ No high availability or fault tolerance
- ⚠️ No advanced security (passwords hashed, but basic auth)
- ⚠️ No observability beyond console logging
- ⚠️ No cloud deployment

## POC Development Workflow

### Phase 1: Setup (Day 1)
- Create solution and project structure
- Setup H2 database with EF Core
- Implement basic authentication

### Phase 2: Core Services (Days 2-5)
- Implement Account, Card, Transaction services
- Map COBOL business logic to Java methods
- Write unit tests

### Phase 3: UI Development (Days 6-8)
- Create Angular pages for key flows
- Implement authentication UI
- Build CRUD pages for accounts/cards/transactions

### Phase 4: Validation (Days 9-10)
- Test all business logic against COBOL behavior
- Run batch processes (interest calculation, posting)
- Demonstrate to stakeholders

## Success Criteria

### Technical Validation
- [ ] All authentication flows work correctly
- [ ] Account CRUD operations match COBOL behavior
- [ ] Interest calculation produces identical results
- [ ] Transaction posting updates balances correctly
- [ ] Unit test coverage > 70%

### Business Validation
- [ ] Stakeholders can use the POC application
- [ ] UI flows match existing COBOL screens
- [ ] All business rules are enforced
- [ ] Error handling is clear

## What Happens After POC?

If POC validation is successful:

1. **Document Findings**: Lessons learned, complexity areas
2. **Refine Architecture**: Use POC insights to improve final architecture
3. **Begin Detailed Specs**: Create production specifications
4. **Plan Production Build**: Timeline, staffing, milestones
5. **Start Final Implementation**: Build with production architecture

See `../overview.md` for final production architecture.

## Related Documentation

### POC Documentation (this folder)
- `overview.md` - POC architecture details
- `technology-stack.md` - POC technology choices
- `solution-structure.md` - POC project structure
- `patterns/` - POC design patterns

### Final Architecture (parent folder)
- `../overview.md` - Production architecture
- `../technology-stack.md` - Production technologies
- `../solution-structure.md` - Production structure
- `../adrs/` - Architecture decisions
- `../patterns/` - Production patterns

### Analysis & Requirements
- `../../analysis/cobol/` - COBOL source analysis
- `../../analysis/architecture/` - Business requirements
- `../../analysis/detailed/` - Detailed specifications

### State Tracking
- `../../state/modernization-state.md` - Overall progress
- `../../state/component-status.md` - Component tracking
- `../../state/decision-log.md` - Decision history

## Questions?

**When to use POC architecture?**
- For initial validation and proof-of-concept work
- When you need quick results (< 2 weeks)
- To demonstrate feasibility to stakeholders

**When to use final architecture?**
- For production-ready implementation
- After POC validation is successful
- When building for scale, resilience, and cloud deployment

**Can POC code go to production?**
- No - POC is for validation only
- Core business logic can be reused
- Architecture must be rebuilt for production

---

**Remember**: POC is a learning tool. Keep it simple, prove the concept, then build production-ready with final architecture.
