# Documentation Hierarchy

This directory contains all documentation for the CardDemo COBOL modernization project, organized by agent responsibility and project phase.

## Directory Structure

```
docs/
├── README.md                           # This file
├── state/                              # State tracking and progress
│   ├── modernization-state.md         # Current state of modernization
│   ├── component-status.md            # Status of each component
│   └── decision-log.md                # Architecture decisions log
├── analysis/                           # Analysis phase outputs
│   ├── cobol/                        # COBOL Analyst outputs
│   │   ├── programs/                 # Program-by-program analysis
│   │   │   ├── PROG-COSGN00C.md
│   │   │   ├── PROG-CBACT01C.md
│   │   │   └── ...
│   │   ├── copybooks/                # Copybook analysis
│   │   │   ├── COPY-COCOM01Y.md
│   │   │   ├── COPY-CVACT01Y.md
│   │   │   └── ...
│   │   ├── screens/                  # Screen (BMS) analysis
│   │   │   ├── SCREEN-COSGN00.md
│   │   │   └── ...
│   │   ├── jobs/                     # Batch job analysis
│   │   │   ├── JOB-CBACT04.md
│   │   │   └── ...
│   │   └── summary/                  # Summary documents
│   │       ├── module-map.md
│   │       └── data-dictionary.md
│   ├── architecture/                  # Architecture Analyst outputs
│   │   ├── use-cases/                # Business use cases
│   │   │   ├── UC-001-authentication.md
│   │   │   ├── UC-002-account-management.md
│   │   │   └── ...
│   │   ├── modules/                  # High-level modules
│   │   │   ├── MOD-001-authentication-module.md
│   │   │   ├── MOD-002-account-module.md
│   │   │   └── ...
│   │   ├── data-flows/               # Data flow diagrams
│   │   │   ├── DF-001-transaction-posting.md
│   │   │   └── ...
│   │   └── opportunities/            # Modernization opportunities
│   │       ├── OPP-001-api-gateway.md
│   │       └── ...
│   └── detailed/                     # Detailed Analyst outputs
│       ├── specifications/           # Detailed specifications
│       │   ├── SPEC-001-create-account.md
│       │   ├── SPEC-002-post-transaction.md
│       │   └── ...
│       ├── data-models/              # Data model definitions
│       │   ├── DM-001-account-entity.md
│       │   ├── DM-002-transaction-entity.md
│       │   └── ...
│       ├── flows/                    # Detailed program flows
│       │   ├── FLOW-001-CBTRN02C-posting.md
│       │   └── ...
│       └── mappings/                 # COBOL to modern mappings
│           ├── MAP-001-cobol-to-csharp-types.md
│           └── ...
├── architecture/                      # Architect outputs
│   ├── overview.md                   # Architecture overview
│   ├── technology-stack.md           # Technology selections
│   ├── solution-structure.md         # Solution organization
│   ├── patterns/                     # Design patterns
│   │   ├── PATTERN-001-cqrs.md
│   │   ├── PATTERN-002-repository.md
│   │   └── ...
│   ├── adrs/                         # Architecture Decision Records
│   │   ├── ADR-001-microservices-approach.md
│   │   ├── ADR-002-database-strategy.md
│   │   └── ...
│   └── guidelines/                   # Development guidelines
│       ├── coding-standards.md
│       ├── security-guidelines.md
│       └── ...
├── implementation/                    # Developer outputs
│   ├── features/                     # Feature implementation docs
│   │   ├── FEAT-001-account-creation.md
│   │   ├── FEAT-002-transaction-posting.md
│   │   └── ...
│   ├── components/                   # Component documentation
│   │   ├── COMP-001-account-service.md
│   │   └── ...
│   └── api/                          # API documentation
│       ├── accounts-api.md
│       ├── transactions-api.md
│       └── openapi.yaml
└── testing/                          # Test Manager outputs
    ├── strategy/                     # Test strategies
    │   └── test-strategy.md
    ├── plans/                        # Test plans
    │   ├── PLAN-001-account-module.md
    │   ├── PLAN-002-transaction-module.md
    │   └── ...
    ├── cases/                        # Test case definitions
    │   ├── TC-001-create-account.md
    │   └── ...
    ├── reports/                      # Test execution reports
    │   ├── REPORT-2025-11-19-sprint-1.md
    │   └── ...
    └── metrics/                      # Quality metrics
        └── quality-dashboard.md
```

## Agent Input/Output Mapping

### 1. COBOL Analyst Agent

**Reads From**:
- `app/cbl/*.cbl` - COBOL source programs
- `app/cpy/*.cpy` - COBOL copybooks
- `app/cpy-bms/*.cpy` - BMS-generated copybooks
- `app/bms/*.bms` - Screen definitions
- `app/jcl/*.jcl` - Batch job definitions
- `docs/state/cobol-analysis-tracker.md` - File analysis status

**Writes To**:
- `docs/analysis/cobol/programs/PROG-{program-name}.md`
- `docs/analysis/cobol/copybooks/COPY-{copybook-name}.md`
- `docs/analysis/cobol/screens/SCREEN-{screen-name}.md`
- `docs/analysis/cobol/jobs/JOB-{job-name}.md`
- `docs/analysis/cobol/summary/module-map.md`
- `docs/analysis/cobol/summary/data-dictionary.md`
- `docs/state/cobol-analysis-tracker.md` (updates status)

**File Naming Convention**:
- Programs: `PROG-{PROGRAM-NAME}.md` (e.g., `PROG-COSGN00C.md`)
- Copybooks: `COPY-{COPYBOOK-NAME}.md` (e.g., `COPY-COCOM01Y.md`)
- Screens: `SCREEN-{SCREEN-NAME}.md` (e.g., `SCREEN-COSGN00.md`)
- Jobs: `JOB-{JOB-NAME}.md` (e.g., `JOB-CBACT04.md`)

### 2. Architecture Analyst Agent

**Reads From**:
- `docs/analysis/cobol/**/*.md` - COBOL analysis outputs
- `app/cbl/*.cbl` - COBOL source programs (if needed)
- `app/cpy/*.cpy` - COBOL copybooks (if needed)
- `app/bms/*.bms` - Screen definitions (if needed)
- `app/jcl/*.jcl` - Batch job definitions (if needed)

**Writes To**:
- `docs/analysis/architecture/use-cases/UC-{id}-{name}.md`
- `docs/analysis/architecture/modules/MOD-{id}-{name}.md`
- `docs/analysis/architecture/data-flows/DF-{id}-{name}.md`
- `docs/analysis/architecture/opportunities/OPP-{id}-{name}.md`
- `docs/state/component-status.md` (updates status)

**File Naming Convention**:
- Use Cases: `UC-{3-digit-id}-{kebab-case-name}.md` (e.g., `UC-001-user-authentication.md`)
- Modules: `MOD-{3-digit-id}-{kebab-case-name}.md` (e.g., `MOD-001-account-management.md`)
- Data Flows: `DF-{3-digit-id}-{kebab-case-name}.md` (e.g., `DF-001-transaction-posting.md`)
- Opportunities: `OPP-{3-digit-id}-{kebab-case-name}.md` (e.g., `OPP-001-event-driven-architecture.md`)

### 3. Detailed Analyst Agent

**Reads From**:
- `docs/analysis/architecture/use-cases/*.md` - High-level use cases
- `docs/analysis/architecture/modules/*.md` - Module definitions
- `docs/analysis/cobol/**/*.md` - COBOL analysis outputs
- `app/cbl/*.cbl` - COBOL source (for detailed analysis)
- `app/cpy/*.cpy` - COBOL copybooks (for data structures)
- `docs/state/component-status.md` - Current component status

**Writes To**:
- `docs/analysis/detailed/specifications/SPEC-{id}-{name}.md`
- `docs/analysis/detailed/data-models/DM-{id}-{entity-name}.md`
- `docs/analysis/detailed/flows/FLOW-{id}-{program-name}.md`
- `docs/analysis/detailed/mappings/MAP-{id}-{mapping-type}.md`
- `docs/state/component-status.md` (updates status)

**File Naming Convention**:
- Specifications: `SPEC-{3-digit-id}-{kebab-case-name}.md` (e.g., `SPEC-001-create-account.md`)
- Data Models: `DM-{3-digit-id}-{entity-name}.md` (e.g., `DM-001-account-entity.md`)
- Flows: `FLOW-{3-digit-id}-{program-name}.md` (e.g., `FLOW-001-CBTRN02C-transaction-posting.md`)
- Mappings: `MAP-{3-digit-id}-{description}.md` (e.g., `MAP-001-cobol-copybook-to-entity.md`)

### 4. Architect Agent

**Reads From**:
- `docs/analysis/architecture/**/*.md` - Architecture analysis
- `docs/analysis/detailed/**/*.md` - Detailed specifications
- `docs/state/modernization-state.md` - Project state
- `docs/architecture/adrs/*.md` - Previous architecture decisions

**Writes To**:
- `docs/architecture/overview.md`
- `docs/architecture/technology-stack.md`
- `docs/architecture/solution-structure.md`
- `docs/architecture/patterns/PATTERN-{id}-{name}.md`
- `docs/architecture/adrs/ADR-{id}-{decision-name}.md`
- `docs/architecture/guidelines/*.md`
- `docs/state/decision-log.md` (updates decisions)
- `docs/state/modernization-state.md` (updates architecture phase)

**File Naming Convention**:
- Patterns: `PATTERN-{3-digit-id}-{kebab-case-name}.md` (e.g., `PATTERN-001-cqrs-implementation.md`)
- ADRs: `ADR-{3-digit-id}-{kebab-case-decision}.md` (e.g., `ADR-001-use-microservices-architecture.md`)

### 5. Developer Agent

**Reads From**:
- `docs/analysis/detailed/specifications/*.md` - Implementation specs
- `docs/analysis/detailed/data-models/*.md` - Data models
- `docs/architecture/overview.md` - Architecture guidelines
- `docs/architecture/technology-stack.md` - Technology choices
- `docs/architecture/solution-structure.md` - Code structure
- `docs/architecture/patterns/*.md` - Design patterns to follow
- `docs/architecture/guidelines/*.md` - Coding standards
- `docs/state/component-status.md` - Implementation status

**Writes To**:
- `src/**/*.cs` - .NET source code
- `tests/**/*.cs` - Test code
- `docs/implementation/features/FEAT-{id}-{name}.md` - Feature documentation
- `docs/implementation/components/COMP-{id}-{name}.md` - Component docs
- `docs/implementation/api/*.md` - API documentation
- `docs/state/component-status.md` (updates implementation status)

**File Naming Convention**:
- Features: `FEAT-{3-digit-id}-{kebab-case-name}.md` (e.g., `FEAT-001-account-creation.md`)
- Components: `COMP-{3-digit-id}-{kebab-case-name}.md` (e.g., `COMP-001-account-service.md`)

### 6. Test Manager Agent

**Reads From**:
- `docs/analysis/architecture/use-cases/*.md` - Use cases (for acceptance criteria)
- `docs/analysis/detailed/specifications/*.md` - Detailed specs (for test criteria)
- `docs/architecture/overview.md` - Architecture (for test scope)
- `docs/implementation/features/*.md` - Feature implementations
- `docs/state/component-status.md` - Component status

**Writes To**:
- `docs/testing/strategy/test-strategy.md`
- `docs/testing/plans/PLAN-{id}-{module-name}.md`
- `docs/testing/cases/TC-{id}-{test-case-name}.md`
- `docs/testing/reports/REPORT-{date}-{sprint-name}.md`
- `docs/testing/metrics/quality-dashboard.md`
- `docs/state/component-status.md` (updates testing status)

**File Naming Convention**:
- Plans: `PLAN-{3-digit-id}-{kebab-case-module}.md` (e.g., `PLAN-001-account-module-testing.md`)
- Test Cases: `TC-{4-digit-id}-{kebab-case-description}.md` (e.g., `TC-0001-create-account-happy-path.md`)
- Reports: `REPORT-{YYYY-MM-DD}-{sprint-or-milestone}.md` (e.g., `REPORT-2025-11-19-sprint-1.md`)

## State Management Files

### modernization-state.md
Tracks the overall progress of the modernization effort.

**Format**:
```markdown
# Modernization State

**Last Updated**: 2025-11-19
**Current Phase**: Analysis
**Overall Progress**: 15%

## Phase Status
- [x] Initial Analysis
- [ ] Architecture Definition (In Progress)
- [ ] Detailed Specification
- [ ] Implementation
- [ ] Testing
- [ ] Deployment

## Current Focus
Currently analyzing: Transaction Processing Module (CBTRN02C)

## Next Steps
1. Complete use case analysis for batch processing
2. Define data models for transaction entities
3. Design microservices architecture
```

### component-status.md
Tracks the status of each component/module being modernized.

**Format**:
```markdown
# Component Status

## MOD-001: Authentication Module

**COBOL Programs**: COSGN00C  
**Status**: Specification Complete  
**Progress**: 40%

| Phase | Status | Document | Last Updated |
|-------|--------|----------|--------------|
| Use Case Analysis | ✅ Complete | UC-001-user-authentication.md | 2025-11-15 |
| Detailed Spec | ✅ Complete | SPEC-001-authenticate-user.md | 2025-11-16 |
| Architecture | ✅ Complete | COMP-001-auth-service.md | 2025-11-17 |
| Implementation | 🔄 In Progress | src/CardDemo.AuthService/ | 2025-11-18 |
| Testing | ⏳ Not Started | - | - |

**Assigned To**: Developer Agent  
**Blockers**: None
```

### decision-log.md
Records all architecture and design decisions.

**Format**:
```markdown
# Decision Log

## ADR-001: Use Microservices Architecture
**Date**: 2025-11-15  
**Status**: Accepted  
**Context**: Need to modernize monolithic COBOL application  
**Decision**: Adopt microservices architecture with domain-driven design  
**Consequences**: Independent deployments, better scalability, increased complexity

[Link to full ADR](../architecture/adrs/ADR-001-use-microservices-architecture.md)
```

## Document Templates

Each document type has a standardized template to ensure consistency.

### Use Case Template
See: `docs/analysis/architecture/use-cases/_TEMPLATE.md`

### Specification Template
See: `docs/analysis/detailed/specifications/_TEMPLATE.md`

### ADR Template
See: `docs/architecture/adrs/_TEMPLATE.md`

### Test Plan Template
See: `docs/testing/plans/_TEMPLATE.md`

## Workflow Example

### Scenario: Modernize Account Creation Feature

1. **COBOL Analyst** analyzes relevant COBOL files
   - Reads: `app/cbl/CBACT01C.cbl`, `app/cpy/CVACT01Y.cpy`
   - Writes: `docs/analysis/cobol/programs/PROG-CBACT01C.md`
   - Writes: `docs/analysis/cobol/copybooks/COPY-CVACT01Y.md`
   - Updates: `docs/state/cobol-analysis-tracker.md` (marks files analyzed)

2. **Architecture Analyst** analyzes COBOL programs for use cases
   - Reads: `docs/analysis/cobol/programs/PROG-CBACT01C.md`
   - Reads: `docs/analysis/cobol/copybooks/COPY-CVACT01Y.md`
   - Writes: `docs/analysis/architecture/use-cases/UC-002-account-creation.md`
   - Updates: `docs/state/component-status.md` (adds MOD-002)

3. **Detailed Analyst** creates specifications
   - Reads: `docs/analysis/architecture/use-cases/UC-002-account-creation.md`
   - Reads: `app/cpy/CVACT01Y.cpy` (for data structures)
   - Writes: `docs/analysis/detailed/specifications/SPEC-002-create-account.md`
   - Writes: `docs/analysis/detailed/data-models/DM-001-account-entity.md`
   - Updates: `docs/state/component-status.md` (marks spec complete)

4. **Architect** defines solution architecture
   - Reads: `docs/analysis/detailed/specifications/SPEC-002-create-account.md`
   - Reads: `docs/architecture/overview.md` (existing architecture)
   - Writes: `docs/architecture/patterns/PATTERN-002-account-repository.md`
   - Writes: `docs/implementation/components/COMP-002-account-service.md`
   - Updates: `docs/state/decision-log.md`

5. **Developer** implements feature
   - Reads: `docs/analysis/detailed/specifications/SPEC-002-create-account.md`
   - Reads: `docs/architecture/solution-structure.md`
   - Reads: `docs/architecture/patterns/PATTERN-001-cqrs-implementation.md`
   - Writes: `src/CardDemo.AccountService/**/*.cs`
   - Writes: `tests/CardDemo.AccountService.Tests/**/*.cs`
   - Writes: `docs/implementation/features/FEAT-002-account-creation.md`
   - Updates: `docs/state/component-status.md` (marks implementation complete)

6. **Test Manager** validates quality
   - Reads: `docs/analysis/detailed/specifications/SPEC-002-create-account.md`
   - Reads: `docs/implementation/features/FEAT-002-account-creation.md`
   - Writes: `docs/testing/plans/PLAN-002-account-creation-testing.md`
   - Writes: `docs/testing/cases/TC-0010-create-account-valid.md`
   - Executes tests and writes: `docs/testing/reports/REPORT-2025-11-19-account-module.md`
   - Updates: `docs/state/component-status.md` (marks testing complete)

## Context Management Strategy

### Problem
AI agents need context to work effectively, but loading the entire COBOL codebase and all documentation exceeds optimal context windows.

### Solution: Progressive Context Loading

**Phase 1: State Check** (Always loaded)
- `docs/state/modernization-state.md` - Know where we are
- `docs/state/component-status.md` - Know what's been done

**Phase 2: Relevant Documents** (Loaded based on task)
- Agent reads only the documents relevant to current task
- Use component status to identify completed prerequisites

**Phase 3: Source Code** (Loaded as needed)
- Only load specific COBOL programs being analyzed
- Reference line numbers to limit scope

### Example: Developer Implementing Account Creation

**Minimal Context**:
1. `docs/state/component-status.md` → See MOD-002 is ready for implementation
2. `docs/analysis/detailed/specifications/SPEC-002-create-account.md` → Get requirements
3. `docs/architecture/solution-structure.md` → Know where to put code
4. `docs/architecture/patterns/PATTERN-001-cqrs-implementation.md` → Follow pattern

**Total**: ~4 documents instead of entire codebase

## Best Practices

1. **Always Update State**: Every agent must update relevant state files
2. **Follow Naming Conventions**: Consistent file names enable easy discovery
3. **Link Documents**: Cross-reference related documents with relative paths
4. **Version Control**: All documentation is version controlled with code
5. **Incremental Progress**: Update status after each component completion
6. **Clear Dependencies**: Document prerequisites in component status
7. **Search by ID**: Use document IDs (UC-001, SPEC-001) for easy reference

## Quick Reference

| Agent | Primary Reads | Primary Writes | State Updates |
|-------|---------------|----------------|---------------|
| COBOL Analyst | `app/cbl/*.cbl`, `app/cpy/*.cpy` | `docs/analysis/cobol/**` | `cobol-analysis-tracker.md` |
| Architecture Analyst | `docs/analysis/cobol/**` | `docs/analysis/architecture/**` | `component-status.md` |
| Detailed Analyst | `docs/analysis/architecture/**` | `docs/analysis/detailed/**` | `component-status.md` |
| Architect | `docs/analysis/**` | `docs/architecture/**` | `decision-log.md` |
| Developer | `docs/architecture/**`, `docs/analysis/detailed/**` | `src/**`, `docs/implementation/**` | `component-status.md` |
| Test Manager | `docs/analysis/**`, `docs/implementation/**` | `docs/testing/**` | `component-status.md` |

## Getting Started

To begin modernization of a new component:

1. Create entry in `docs/state/component-status.md`
2. Architecture Analyst creates use case in `docs/analysis/architecture/use-cases/`
3. Update component status to "Use Case Complete"
4. Proceed to next agent in workflow

For more information, see the agent-specific documentation in `.github/agents/`.
