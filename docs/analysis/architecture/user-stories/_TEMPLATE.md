# US-{ID}: {Short Title}

**Document ID**: US-{4-digit-id}  
**Created**: YYYY-MM-DD  
**Last Updated**: YYYY-MM-DD  
**Author**: Architecture Analyst  
**Status**: [Draft | Ready | In Progress | Complete]

## User Story

**As a** {user persona}  
**I want to** {perform action}  
**So that** {business value/goal I can achieve}

## Overview

**Epic/Feature**: [Reference to larger feature if applicable]  
**Business Requirement**: [BR-XXX reference]  
**Use Case**: [UC-XXX reference]  
**Priority**: [High | Medium | Low]  
**Complexity**: [Low | Medium | High]  
**Story Points**: [Estimate if using agile]

## Source

**COBOL Program**: `path/to/program.cbl`  
**COBOL Analysis**: [Link to COBOL analysis document]  
**Relevant Lines**: [Line numbers in COBOL where this functionality exists]

## Business Context

[1-2 paragraphs explaining the business value and context for this user story]

## Acceptance Criteria

### Scenario 1: {Primary Happy Path}

**Given** {the user is in a certain context/state}  
**When** {the user performs an action}  
**Then** {the expected outcome occurs}  
**And** {additional expected outcome if applicable}

### Scenario 2: {Alternative Flow}

**Given** {different context}  
**When** {user action}  
**Then** {different expected outcome}

### Scenario 3: {Error/Exception Case}

**Given** {error condition context}  
**When** {user action that triggers error}  
**Then** {system responds with appropriate error handling}  
**And** {user can recover or is guided appropriately}

## Business Rules Applied

- **Rule {ID}**: [Brief statement of rule] - [Reference to BR document]
- **Rule {ID}**: [Another rule]

## UI/UX Considerations

### Display Elements
- [What information must be displayed to the user]
- [Format/layout considerations]

### Input Elements
- [What inputs user must provide]
- [Validation requirements]

### Interactions
- [User interactions required - buttons, forms, navigation]
- [Feedback mechanisms - messages, confirmations]

### Accessibility
- [Specific accessibility requirements for this story]

## Data Requirements

**Inputs**:
- **{Field/Data}**: [Format, validation, source]

**Outputs**:
- **{Field/Data}**: [Format, calculation, display]

**Entities Involved**:
- [Entity name from BR document]

## Technical Constraints

[Only include technology-agnostic constraints]
- [e.g., "Must support offline capability"]
- [e.g., "Must process within 2 seconds"]

## Dependencies

**Requires**:
- [US-XXX: Another user story that must be complete first]
- [Data/API availability]

**Blocks**:
- [US-YYY: Story that depends on this one]

## Definition of Done

- [ ] Functional requirement implemented
- [ ] All acceptance criteria pass
- [ ] Unit tests written and passing
- [ ] Integration tests passing
- [ ] Business rules validated
- [ ] UI/UX reviewed and approved
- [ ] Accessibility requirements met
- [ ] Performance requirements met
- [ ] Documentation updated
- [ ] Code reviewed
- [ ] Deployed to test environment
- [ ] User acceptance tested

## Test Scenarios

### Test 1: {Scenario Name}
**Pre-conditions**: [Setup required]  
**Steps**: [User actions to perform]  
**Expected Result**: [What should happen]

### Test 2: {Another Scenario}
[Same structure]

## Notes

[Any additional notes, edge cases, or considerations]

## Related Stories

- **Parent**: [US-XXX or Feature reference]
- **Related**: [US-YYY: Related functionality]
- **Child**: [US-ZZZ: Sub-story if this was split]

## Modernization Notes

### COBOL Behavior
[Brief description of how COBOL currently implements this - 2-3 sentences]

### Modern Approach Considerations
- [How this differs in a web context]
- [Improvements over legacy implementation]

## Change Log

| Date | Change | Author |
|------|--------|--------|
| YYYY-MM-DD | Initial version | Architecture Analyst |
