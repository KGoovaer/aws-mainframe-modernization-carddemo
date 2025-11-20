# Architecture Decision Log

This file provides a summary of all Architecture Decision Records (ADRs) for the CardDemo modernization project.

**Last Updated**: 2025-11-20

## Active Decisions

| ID | Title | Date | Status | Impact |
|----|-------|------|--------|--------|
| ADR-001 | Use Modular Monolith over Microservices | 2025-11-20 | Accepted | High - Defines overall application architecture |
| ADR-002 | Database Strategy - Single Database with Schema-per-Module | 2025-11-20 | Accepted | High - Defines data architecture and persistence strategy |
| ADR-003 | CQRS with MediatR for Application Layer | 2025-11-20 | Accepted | High - Defines application layer structure |

## All Decisions

---

## ADR Template

When creating new ADRs in `docs/architecture/adrs/`, use this format:

```markdown
# ADR-{id}: {Title}

**Date**: YYYY-MM-DD  
**Status**: [Proposed | Accepted | Deprecated | Superseded]  
**Deciders**: [Names/Roles]  
**Technical Story**: [ticket/issue reference]

## Context

What is the issue that we're seeing that is motivating this decision or change?

## Decision Drivers

* [driver 1]
* [driver 2]
* [driver 3]

## Considered Options

* [option 1]
* [option 2]
* [option 3]

## Decision Outcome

Chosen option: "[option 1]", because [justification].

### Positive Consequences

* [e.g., improvement of quality attribute satisfaction, follow-up decisions required, …]
* …

### Negative Consequences

* [e.g., compromising quality attribute, follow-up decisions required, …]
* …

## Pros and Cons of the Options

### [option 1]

[example | description | pointer to more information | …]

* Good, because [argument a]
* Good, because [argument b]
* Bad, because [argument c]
* …

### [option 2]

[example | description | pointer to more information | …]

* Good, because [argument a]
* Good, because [argument b]
* Bad, because [argument c]
* …

## Links

* [Link type] [Link to ADR]
* [Related decisions, documents, or resources]
```

---

## Decision History

### 2025-11-20: Architecture Definition Phase Complete

**Summary**: Core architecture decisions made and documented  
**Impact**: Establishes technical direction for modernization  
**Documents**: 
- `architecture/overview.md` - Complete architecture overview
- `architecture/technology-stack.md` - Technology selections with rationale
- `architecture/solution-structure.md` - Solution organization and folder structure
- `architecture/adrs/ADR-001-use-modular-monolith-architecture.md` - Modular monolith decision
- `architecture/adrs/ADR-003-cqrs-with-mediatr.md` - CQRS implementation decision
- `architecture/patterns/PATTERN-001-cqrs-implementation.md` - CQRS pattern documentation

**Key Decisions**:
- **Modular Monolith**: Start with modular monolith, not microservices
- **Clean Architecture**: Domain-driven design with clear layer boundaries
- **CQRS with MediatR**: Command/query separation in application layer
- **.NET 10**: Latest LTS platform
- **Azure Container Apps**: Serverless container hosting
- **Azure SQL Database**: Single database with schema-per-module
- **Azure Service Bus**: Event-driven asynchronous communication

### 2025-11-19: Project Initialization

**Summary**: Project structure and agent workflow established  
**Impact**: Foundation for all modernization work  
**Documents**: `/docs/README.md`, `/docs/state/`

---

## Notes

- All major architecture decisions should have a corresponding ADR
- ADRs are immutable once accepted (create new ADR to supersede)
- Link related ADRs to show decision evolution
- Keep this log updated when new ADRs are created
