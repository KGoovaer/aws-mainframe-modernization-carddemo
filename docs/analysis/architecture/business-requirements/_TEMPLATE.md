# BR-{ID}: {Requirement Area}

**Document ID**: BR-{3-digit-id}  
**Created**: YYYY-MM-DD  
**Last Updated**: YYYY-MM-DD  
**Author**: Architecture Analyst  
**Status**: [Draft | Review | Approved]

## Overview

**Business Capability**: [What business function this supports]  
**Priority**: [High | Medium | Low]  
**Component**: [MOD-XXX reference]

## Source COBOL Programs

- **Primary**: `path/to/program.cbl` - [Brief description]
- **Related**: `path/to/related1.cbl`, `path/to/related2.cbl`
- **Copybooks**: `path/to/copybook.cpy` (if applicable)

## Business Context

[2-3 paragraphs describing why this capability exists, what business problem it solves, and what value it provides to the organization and users]

## Functional Requirements

### FR-{ID}.1: {Requirement Title}

**Description**: [Clear statement of what the system must do - technology-agnostic]

**User Need**: [Why this is needed from user/business perspective]

**Business Justification**: [Value, compliance, or operational reason]

**Source**: [Reference to COBOL analysis document and line numbers]

**Acceptance Criteria**:
- [ ] [Measurable criterion 1]
- [ ] [Measurable criterion 2]

### FR-{ID}.2: {Another Requirement}

[Same structure as above]

### FR-{ID}.3: {Yet Another Requirement}

[Continue for all functional requirements]

## Business Rules

### Rule {ID}: {Rule Name}

**Statement**: [Clear, concise rule statement in business terms]

**Rationale**: [Why this rule exists - business reason]

**Conditions**: [When this rule applies]

**Actions**: [What happens when rule is triggered]

**Source**: [COBOL program and line reference - e.g., COSGN00C lines 245-267]

**Priority**: [Critical | High | Medium | Low]

### Rule {ID}: {Another Rule}

[Continue for all business rules extracted from COBOL]

## Data Requirements

### Entity: {Business Entity Name}

**Purpose**: [What this entity represents in the business domain]

**Key Attributes**:
- **{Attribute}**: [Description, type (text/number/date), constraints]
- **{Attribute}**: [Description]

**Relationships**: [How this entity relates to other business entities]

**Business Rules**: [References to rules that apply to this entity]

**Source**: [COBOL copybook reference]

### Entity: {Another Business Entity}

[Continue for all entities]

## User Personas Affected

### {Persona Name} (e.g., Customer Service Representative)

**Role Description**: [What they do in the organization]

**Needs from This Capability**:
- [Specific need 1]
- [Specific need 2]

**Interaction Frequency**: [Daily | Weekly | Monthly | As needed]

**Technical Proficiency**: [Low | Medium | High]

### {Another Persona}

[Continue for all personas]

## Non-Functional Requirements

### Performance
- **Response Time**: [Expected max response time for user interactions]
- **Throughput**: [Expected transaction volume]
- **Concurrent Users**: [Expected peak concurrent usage]

### Security
- **Authentication**: [What authentication is required]
- **Authorization**: [What access controls are needed]
- **Data Protection**: [Sensitive data handling requirements]
- **Audit**: [What needs to be logged/tracked]

### Usability
- **Accessibility**: [WCAG compliance level, screen reader support, etc.]
- **Browser Support**: [Which browsers must be supported]
- **Mobile Support**: [Mobile responsive requirements]
- **User Experience**: [Key UX requirements]

### Compliance
- [Any regulatory or compliance requirements]

### Availability
- **Uptime**: [Expected availability percentage]
- **Recovery Time**: [Maximum tolerable downtime]

### Scalability
- **Growth**: [Expected growth over time]
- **Peak Handling**: [How to handle peak periods]

## Integration Requirements

**External Systems**:
- [System name]: [Integration purpose and frequency]

**Data Exchanges**:
- [What data is exchanged with other systems]

**APIs Required**:
- [What APIs this capability must provide or consume]

## Success Criteria

- [ ] [Measurable business outcome 1]
- [ ] [Measurable business outcome 2]
- [ ] [Measurable business outcome 3]
- [ ] [User satisfaction metric]
- [ ] [Performance metric]

## Dependencies

**Requires**:
- [BR-XXX: Other business requirement]
- [Data migration from VSAM files]

**Blocks**:
- [BR-YYY: Other requirement that depends on this]

## Assumptions

- [Assumption about business environment or users]
- [Technology assumption - but keep technology-agnostic]

## Open Questions

- [ ] **Q1**: [Question that needs business stakeholder input]
- [ ] **Q2**: [Another question]

## Related Use Cases

- [UC-XXX: Use case title]
- [UC-YYY: Another use case]

## Related User Stories

- [US-XXX: User story title]
- [US-YYY: Another user story]

## Modernization Notes

### Current COBOL Implementation Summary
[Brief 2-3 sentence summary of how COBOL currently implements this]

### Key Considerations for Modern Implementation
- [Consideration 1 - e.g., "Multiple users may work simultaneously (vs. single CICS task)"]
- [Consideration 2 - e.g., "Web forms vs. 3270 screen constraints"]
- [Consideration 3]

### Opportunities
- [Opportunity to improve on legacy implementation]
- [Another opportunity]

## Change Log

| Date | Change | Author |
|------|--------|--------|
| YYYY-MM-DD | Initial version | Architecture Analyst |
