# GitHub Copilot Instructions - CardDemo Modernization

This file contains general instructions when working on the CardDemo COBOL-to-.NET modernization project.

## Project Overview

This is an AI-assisted modernization of the AWS CardDemo mainframe application (COBOL/CICS/VSAM) to modern .NET microservices using Clean Architecture, CQRS, DDD patterns and modern web technologies.

## Core Principles

1. **State-Driven Context Management**: Always read state files first to understand project status
2. **Documentation-First Approach**: Analysis and architecture work produces markdown, not code
3. **Separation of Concerns**: Each agent has specific responsibilities and outputs
4. **Progressive Loading**: Load only what's needed for the current task
5. **Traceability**: Maintain clear links between COBOL source and modern implementation

## Context Management Strategy

**ALWAYS start by reading these files** (minimal context load):

1. `docs/state/modernization-state.md` - Overall project progress and current phase
2. `docs/state/component-status.md` - Per-component status and workflow tracking

This prevents needing to scan the entire codebase to understand project status.

## Agent System

This project uses specialized agents working in a pipeline. Each agent is documented in `.github/agents/`:

- **COBOL Analyst** - Analyzes COBOL source files systematically
- **Application Architect** - Translates COBOL to business requirements and use cases
- **Detailed Analyst** - Creates detailed technical specifications
- **Software Architect** - Defines modern system architecture
- **Developer** - Implements .NET code following specifications
- **Test Manager** - Defines test strategy and creates test plans

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

- **.NET 8+** with C#
- **Clean Architecture** (Domain, Application, Infrastructure, Presentation)
- **CQRS** 
- **Domain-Driven Design** patterns
- **Entity Framework Core** for data access
- **xUnit** for testing
- **Docker** for containerization
- **Azure** for cloud deployment

## Quality Standards

- All code must have unit tests
- Follow C# coding conventions
- Use async/await patterns
- Implement proper error handling
- Include XML documentation comments
- Follow SOLID principles

## Links to Detailed Documentation

- **Full Documentation Guide**: `/docs/README.md`
- **Setup Summary**: `/docs/SETUP-SUMMARY.md`
- **State Tracking**: `/docs/state/*.md`

