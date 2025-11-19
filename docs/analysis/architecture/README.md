# Architecture Analysis

This directory contains business requirements, use cases, and user stories extracted from COBOL analysis by the Architecture Analyst agent.

## Purpose

The Architecture Analyst translates technical COBOL program behavior into **business requirements** that describe WHAT the modernized system must do, without specifying HOW it will be built.

## Key Principle: Technology Abstraction

All documents in this directory are **technology-agnostic**:
- ❌ No mainframe jargon (CICS, VSAM, 3270, JCL)
- ❌ No implementation details (REST APIs, databases, microservices)
- ✅ Business capabilities and user needs
- ✅ User journeys in a web context
- ✅ Testable outcomes and acceptance criteria

## Directory Structure

```
architecture/
├── business-requirements/    # BR-XXX documents - comprehensive functional requirements
├── use-cases/               # UC-XXX documents - user interaction scenarios
├── user-stories/            # US-XXX documents - agile user stories
├── modules/                 # MOD-XXX documents - logical module groupings
├── data-flows/             # DF-XXX documents - data movement patterns
└── opportunities/          # OPP-XXX documents - modernization opportunities
```

## Document Types

### Business Requirements (BR)

**Purpose**: Comprehensive functional requirements for a business capability  
**Scope**: One BR document per major component/module  
**Focus**: WHAT the system must do

**Key Sections**:
- Functional Requirements (FR-XXX.1, FR-XXX.2, etc.)
- Business Rules (with COBOL source references)
- Data Requirements (business entities and attributes)
- User Personas
- Non-Functional Requirements (performance, security, usability)
- Success Criteria

**Example**: `BR-001-user-authentication-requirements.md`

### Use Cases (UC)

**Purpose**: Describe user interactions with the web-based system  
**Scope**: One UC per distinct user journey or interaction  
**Focus**: User goals and system responses

**Key Sections**:
- Main Success Scenario (step-by-step flow)
- Alternative Flows (different paths to success)
- Exception Flows (error handling)
- Acceptance Criteria
- Business Rules Applied

**Example**: `UC-001-user-login.md`, `UC-002-view-account-balance.md`

### User Stories (US)

**Purpose**: Granular, implementable features in agile format  
**Scope**: Multiple US per use case - small, testable increments  
**Focus**: User value and acceptance criteria

**Format**: "As a {persona}, I want to {action}, so that {value}"

**Key Sections**:
- User Story statement
- Acceptance Criteria (Given/When/Then format)
- Business Rules Applied
- UI/UX Considerations
- Definition of Done

**Example**: `US-001-user-enters-credentials.md`, `US-002-system-validates-password.md`

## Document Hierarchy

```
BR-001: User Authentication Requirements
├── UC-001: User Login
│   ├── US-001: User enters credentials
│   ├── US-002: System validates password
│   ├── US-003: System creates session
│   └── US-004: System displays error for invalid login
├── UC-002: User Logout
│   ├── US-005: User initiates logout
│   └── US-006: System terminates session
└── UC-003: Password Reset
    ├── US-007: User requests password reset
    └── US-008: System sends reset instructions
```

## Relationship to Other Documentation

**Input**: 
- `docs/analysis/cobol/**/*.md` - COBOL Analyst's detailed file analysis

**Output Used By**:
- **Detailed Analyst** - Creates technical specifications from these requirements
- **Architect** - Designs architecture to meet these requirements
- **Developer** - Implements features based on user stories
- **Test Manager** - Creates test cases from acceptance criteria

## Translation Guidelines

The Architecture Analyst follows these translation rules:

| COBOL Concept | Business Requirement |
|---------------|---------------------|
| CICS SEND MAP | System displays information to user |
| CICS RECEIVE MAP | User submits input through form |
| READ file | System retrieves data |
| WRITE/REWRITE file | System saves/updates data |
| CALL subprogram | System performs validation/calculation |
| IF conditions | Business rules and validations |
| PERFORM loops | System processes multiple items |
| Batch job | Scheduled or background processing |

## File Naming Conventions

- **Business Requirements**: `BR-{3-digit-id}-{kebab-case-name}.md`
  - Example: `BR-001-user-authentication-requirements.md`
  
- **Use Cases**: `UC-{3-digit-id}-{kebab-case-name}.md`
  - Example: `UC-001-user-login.md`
  
- **User Stories**: `US-{4-digit-id}-{kebab-case-name}.md`
  - Example: `US-0001-user-enters-credentials.md`
  - Note: User stories use 4 digits due to higher volume

## Templates

Templates are provided for each document type:
- `business-requirements/_TEMPLATE.md`
- `use-cases/_TEMPLATE.md`
- `user-stories/_TEMPLATE.md`

## Quality Checklist

Before considering a component's business requirements complete, verify:

- [ ] All BR documents are technology-agnostic
- [ ] Use cases describe web-based interactions (not 3270 screens)
- [ ] User stories follow "As a...I want...So that" format
- [ ] Acceptance criteria are testable and measurable
- [ ] Business rules are clearly stated and sourced to COBOL
- [ ] No mainframe jargon in requirements
- [ ] Requirements are traceable to COBOL analysis
- [ ] All documents reference source COBOL programs
- [ ] Non-functional requirements are included
- [ ] User personas are identified

## Workflow

1. **COBOL Analyst** completes file analysis for a component
2. **Architecture Analyst** reads COBOL analysis
3. **Architecture Analyst** creates:
   - 1 BR document (comprehensive requirements)
   - 3-5 UC documents (major user interactions)
   - 8-12 US documents (granular features)
4. **Architecture Analyst** updates `component-status.md` to "Business Requirements Complete"
5. **Detailed Analyst** uses these requirements to create technical specifications

## Example: Authentication Module

For MOD-001 (Authentication), the Architecture Analyst produces:

**Business Requirements**:
- `BR-001-user-authentication-requirements.md`

**Use Cases**:
- `UC-001-user-login.md`
- `UC-002-user-logout.md`
- `UC-003-password-reset.md`
- `UC-004-session-management.md`

**User Stories** (examples):
- `US-0001-user-enters-credentials.md`
- `US-0002-system-validates-password.md`
- `US-0003-system-creates-session.md`
- `US-0004-system-displays-login-error.md`
- `US-0005-user-initiates-logout.md`
- `US-0006-system-clears-session.md`
- `US-0007-user-requests-password-reset.md`
- `US-0008-system-validates-email.md`
- `US-0009-system-sends-reset-email.md`
- `US-0010-user-sets-new-password.md`

## Best Practices

1. **Start with COBOL Analysis**: Always read the COBOL Analyst's output first
2. **Focus on Business Value**: Ask "Why does the user need this?" not "How does COBOL do this?"
3. **Abstract Technology**: Translate 3270 screens to web pages conceptually
4. **Business Language**: Write for business stakeholders, not developers
5. **Traceability**: Always reference source COBOL programs and line numbers
6. **Testable**: Every requirement must have measurable acceptance criteria
7. **Complete**: Cover all functional and non-functional requirements
8. **User-Centric**: Think from the user's perspective, not the system's

## Common Patterns

### Online COBOL Programs → Web Pages
- COSGN00C (Sign-on screen) → Login page
- COMEN01C (Menu screen) → Navigation/dashboard
- COCRDLIC (Card list screen) → Data grid/list view
- COCRDUPC (Card update screen) → Edit form

### Batch COBOL Programs → Background Processing
- CBTRN02C (Transaction posting) → Scheduled job
- CBACT04C (Interest calculation) → Automated calculation
- CBSTM03A (Statement generation) → Report generation

### Copybooks → Data Models
- CVACT01Y (Account structure) → Account entity
- CVCRD01Y (Card structure) → Card entity
- CVTRA05Y (Transaction structure) → Transaction entity

## Remember

The Architecture Analyst is the **bridge between legacy COBOL and modern business requirements**. Output must be:
- **Technology-agnostic** - No HOW, only WHAT
- **Business-focused** - Value and capabilities
- **User-centric** - From user's perspective
- **Testable** - Clear acceptance criteria
- **Complete** - Functional + non-functional requirements

**Documents describe WHAT the new system must do, not HOW it will be built.**
