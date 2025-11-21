# GitHub Copilot Instructions - CardDemo Modernization

This file contains general instructions when working on the CardDemo COBOL-to-Java modernization project.

## Project Overview

This is an AI-assisted modernization of the AWS CardDemo mainframe application (COBOL/CICS/VSAM) to modern Java using a **dual-track approach**:

- **POC Track**: Simplified implementation (H2/PostgreSQL, basic patterns, no CQRS) for rapid validation
- **Final Architecture Track**: Production-ready design (Clean Architecture, CQRS, DDD, Azure/AWS)

## Core Principles

1. **Dual-Track Development**: POC first for validation, final architecture for production
2. **State-Driven Context Management**: Always read state files first to understand project status
3. **Documentation-First Approach**: Analysis and architecture work produces markdown, not code
4. **Separation of Concerns**: Each agent has specific responsibilities and outputs
5. **Progressive Loading**: Load only what's needed for the current task
6. **Traceability**: Maintain clear links between COBOL source and modern implementation

## Context Management Strategy

**ALWAYS start by reading these files** (minimal context load):

1. `docs/state/modernization-state.md` - Overall project progress and current phase
2. `docs/state/component-status.md` - Per-component status and workflow tracking

This prevents needing to scan the entire codebase to understand project status.

## Agent System

This project uses specialized agents working in a pipeline. Each agent is documented in `.github/agents/`:

- **COBOL Analyst** - Analyzes COBOL source files systematically
- **Application Architect** - Translates COBOL to business requirements and use cases
- **Software Architect** - Defines architecture (POC mode by default, final mode on request)
- **POC Developer** - Implements simplified POC code (SQLite, basic patterns)
- **Developer** - Implements production code (Clean Architecture, CQRS, DDD)
- **Test Manager** - Defines test strategy and creates test plans

See `agent_pipeline.md` for complete workflow details.

## Documentation Structure

All documentation lives under `/docs`. See `/docs/README.md` for complete hierarchy. Only load relevant sections as needed.

## File Naming Conventions

Use consistent numeric IDs with kebab-case:

- `UC-001-name.md` - Use cases (3 digits)
- `SPEC-001-name.md` - Specifications (3 digits)  
- `ADR-001-name.md` - Architecture decisions (3 digits)
- `TC-0001-name.md` - Test cases (4 digits)

## State Tracking

Every agent must update state files when completing work:
- `docs/state/component-status.md` - Component progress tracking
- `docs/state/modernization-state.md` - Overall project status

## Target Technology Stack

### POC Stack (Default for Initial Development)
- **Java 21** (LTS) with Spring Boot 3.x
- **Simple layered architecture** (Presentation → Business → Data)
- **H2/PostgreSQL** with Spring Data JPA
- **Repository pattern** (Spring Data repositories)
- **Angular 18** for UI
- **JUnit 5** for testing
- **No CQRS, no messaging, no cloud services**

### Final Architecture Stack (Production Target)
- **Java 21** (LTS) with Spring Boot 3.x
- **Clean Architecture** (Domain, Application, Infrastructure, Presentation)
- **CQRS** with Axon Framework or custom implementation
- **Domain-Driven Design** patterns
- **Azure SQL Database / AWS RDS PostgreSQL** with Spring Data JPA
- **Azure Service Bus / AWS SQS** for messaging
- **Azure Container Apps / AWS ECS** for deployment
- **Azure Application Insights / AWS CloudWatch** for observability
- **JUnit 5 + MockMvc** for comprehensive testing

## Quality Standards

- All code must have unit tests
- Follow Java coding conventions (Google Java Style Guide)
- Use CompletableFuture for async patterns
- Implement proper error handling with Spring exception handling
- Include Javadoc documentation
- Follow SOLID principles

## Development Tracks

### When to Use POC Track
- **Default choice** for initial feature development
- Proving business logic correctness
- Validating data models and flows
- Quick demonstrations to stakeholders
- Learning and experimentation

### When to Use Final Architecture Track
- Production-ready implementation
- After POC validation successful
- Building for scale, resilience, observability
- Integrating with cloud services
- When explicitly requested

### Code Location
- **POC Code**: `src/poc/carddemo-poc/`
- **Production Code**: `src/` (main project structure)
- **POC Docs**: `docs/architecture/poc/` and `docs/implementation/poc/`
- **Production Docs**: `docs/architecture/` and `docs/implementation/`

## Agent Mode Selection

### Software Architect
- **Default**: POC mode (H2/PostgreSQL, simple patterns)
- **Switch to final**: User says "final architecture", "production architecture", or "target architecture"

### Developer Selection
- **POC Developer**: For POC track features
- **Developer**: For final architecture track features

Both agents work in parallel but on different codebases and with different patterns.

## Links to Detailed Documentation

- **Agent Pipeline**: `/agent_pipeline.md` - Complete agent workflow
- **Full Documentation Guide**: `/docs/README.md`
- **Setup Summary**: `/docs/SETUP-SUMMARY.md`
- **State Tracking**: `/docs/state/*.md`

