# Program Analysis: {PROGRAM-NAME}

**Template Version**: 1.0  
**Last Updated**: {DATE}

## Overview
**Program Name**: {COBOL program name}  
**Source File**: `app/cbl/{filename}.cbl`  
**Type**: Online/Batch/Utility  
**Business Function**: {What this program does}  
**Module**: {Which module it belongs to}  
**Analyzed By**: COBOL Analyst  
**Analysis Date**: {date}

## Purpose
{Detailed description of what the program accomplishes - 2-3 paragraphs}

## Program Structure

### Identification Division
- **Program-ID**: {program-id}
- **Author**: {if present}
- **Date Written**: {if present}

### Environment Division

**File Control**:
| File | Organization | Access | Purpose |
|------|--------------|--------|---------|
| {FILE-NAME} | VSAM/Sequential | Sequential/Random | {Purpose} |

**Working-Storage Section**:
| Variable/Structure | Type | Purpose |
|--------------------|------|---------|
| {VAR-NAME} | {Type} | {Purpose} |

### Procedure Division

**Main Flow**:
1. {Step 1 with line number reference}
2. {Step 2 with line number reference}
3. {Step 3 with line number reference}

**Entry Point**: {Main entry paragraph/section}  
**Exit Point**: {Normal termination point}

## Key Procedures/Paragraphs

### {PARAGRAPH-NAME} (Lines {start}-{end})
**Purpose**: {What this paragraph does}  
**Called By**: {Which paragraphs call this}  
**Calls**: {Which paragraphs this calls}  
**Logic Summary**:
- {Key logic point 1}
- {Key logic point 2}

**Code Flow**:
```
{Pseudo-code or structured description}
```

### {PARAGRAPH-NAME-2} (Lines {start}-{end})
{Repeat for each significant paragraph}

## Data Structures Used

| Copybook | Type | Purpose | Key Fields |
|----------|------|---------|------------|
| {COPYBOOK} | Communication/Entity/Screen | {Purpose} | {Fields} |

**Copybook Dependencies**:
```
COPY {COPYBOOK-1}.     * {Purpose}
COPY {COPYBOOK-2}.     * {Purpose}
```

## File Operations

| File | Type | Operations | Access Method | Purpose |
|------|------|------------|---------------|---------|
| {FILE} | VSAM/Sequential | Read/Write/Update/Delete | Sequential/Random | {Purpose} |

**File Access Patterns**:
- {Pattern description with line references}

## Screen Interactions

| Screen | BMS Map | Operation | Purpose | Key Fields |
|--------|---------|-----------|---------|------------|
| {SCREEN} | {MAP} | Send/Receive | {Purpose} | {Fields} |

**Screen Flow**:
- **Entry**: {How user arrives at this screen}
- **Processing**: {What happens}
- **Exit**: {Where user goes next}

## Business Logic

### Calculations
| Calculation | Lines | Formula/Logic | Purpose |
|-------------|-------|---------------|---------|
| {CALC-NAME} | {xxx-yyy} | {Formula} | {Purpose} |

### Validations
| Validation | Lines | Rule | Error Message |
|------------|-------|------|---------------|
| {VALIDATION} | {xxx-yyy} | {Rule} | {Message} |

### Business Rules
1. **{RULE-NAME}** (Lines {xxx-yyy}): {Description}
2. **{RULE-NAME}** (Lines {xxx-yyy}): {Description}

### Error Handling
- **Error Detection**: {How errors are detected}
- **Error Messages**: {How errors are communicated}
- **Error Recovery**: {Recovery mechanisms}

**Error Codes**:
| Code | Message | Condition |
|------|---------|-----------|
| {CODE} | {MSG} | {Condition} |

## Dependencies

### Called Programs
| Program | Purpose | Parameters | Return Values |
|---------|---------|------------|---------------|
| {PROGRAM} | {Purpose} | {Parameters} | {Returns} |

### Calling Programs
| Program | Context | Purpose |
|---------|---------|---------|
| {PROGRAM} | {Context} | {Why it calls this} |

### Copybooks Included
| Copybook | Lines | Type | Purpose |
|----------|-------|------|---------|
| {COPYBOOK} | {line} | Communication/Entity | {Purpose} |

### External Resources
- **Databases**: {Database connections if any}
- **Message Queues**: {MQ connections if any}
- **External Files**: {External files accessed}

## CICS Commands

| Command | Lines | Purpose | Parameters |
|---------|-------|---------|------------|
| SEND MAP | {xxx} | Display screen | MAP={map}, MAPSET={mapset} |
| RECEIVE MAP | {xxx} | Get user input | MAP={map}, MAPSET={mapset} |
| READ | {xxx} | Read VSAM record | FILE={file}, INTO={area} |

## Integration Points

### Internal
- **Modules**: {Related modules within CardDemo}
- **Shared Data**: {Shared data structures}
- **Common Functions**: {Common utilities used}

### External
- **Systems**: {External systems if any}
- **APIs**: {APIs called if any}
- **Files**: {External file interfaces}

## Control Flow

### Decision Points
| Line | Condition | True Path | False Path |
|------|-----------|-----------|------------|
| {xxx} | {Condition} | {Action} | {Action} |

### Loop Structures
| Type | Lines | Condition | Purpose |
|------|-------|-----------|---------|
| PERFORM UNTIL | {xxx-yyy} | {Condition} | {Purpose} |

### Exception Handling
- {Exception type}: {Handling approach}

## Complexity Assessment

**Cyclomatic Complexity**: {Low/Medium/High}  
**Lines of Code**: {count}  
**Number of Paragraphs**: {count}  
**Number of Decision Points**: {count}

**Key Complexity Factors**:
1. {Factor 1 - e.g., "Multiple nested loops"}
2. {Factor 2 - e.g., "Complex conditional logic"}
3. {Factor 3 - e.g., "Many GOTO statements"}

**Maintainability Index**: {Low/Medium/High}

### Code Quality Observations
- **Strengths**: {Positive aspects}
- **Concerns**: {Areas of concern}
- **Anti-Patterns**: {Legacy patterns that complicate understanding}

## Data Flow

```
Input Sources → Processing → Output Destinations
```

**Detailed Flow**:
1. **Input**: {Source} → {Data}
2. **Processing**: {Transformation/Calculation}
3. **Output**: {Data} → {Destination}

## Transaction Characteristics

**Type**: {Online/Batch}  
**Frequency**: {Per day/hour/etc.}  
**Volume**: {Estimated records/transactions}  
**Response Time**: {Expected response time}  
**Peak Load**: {Peak usage characteristics}

## Modernization Analysis

### Modernization Priority
**Priority**: High/Medium/Low  
**Rationale**: {Why this priority}

### Key Challenges
1. **{CHALLENGE-1}**: {Description and impact}
2. **{CHALLENGE-2}**: {Description and impact}

### Recommended Approach
**Pattern**: {Microservice/API/Function/etc.}  
**Technology**: {Suggested .NET approach}

**Mapping Strategy**:
1. {Mapping approach 1}
2. {Mapping approach 2}

### Modernization Considerations
- **Business Logic**: {How to extract/preserve}
- **Data Access**: {How to modernize}
- **User Interface**: {Modern UI approach}
- **Integration**: {How to integrate with other services}

### Estimated Effort
**Complexity**: {Simple/Moderate/Complex}  
**Estimated Days**: {estimate}  
**Risk Level**: {Low/Medium/High}

## COBOL Code References

### Key Sections
| Lines | Section/Paragraph | Purpose |
|-------|-------------------|---------|
| {xxx-yyy} | {NAME} | {Purpose} |

### Critical Logic
| Lines | Description | Notes |
|-------|-------------|-------|
| {xxx-yyy} | {Logic description} | {Important notes} |

### Areas Requiring Special Attention
1. **Lines {xxx-yyy}**: {Description of complex/critical code}
2. **Lines {xxx-yyy}**: {Description of complex/critical code}

## Testing Considerations

### Test Scenarios
1. **Happy Path**: {Primary success scenario}
2. **Error Cases**: {Expected error conditions}
3. **Edge Cases**: {Boundary conditions}

### Test Data Requirements
- {Data requirement 1}
- {Data requirement 2}

## Documentation References

### Related Analysis Documents
- **Copybooks**: {List of related copybook analysis docs}
- **Screens**: {List of related screen analysis docs}
- **Programs**: {List of related program analysis docs}

### Source Code Location
- **Repository Path**: `app/cbl/{filename}.cbl`
- **Version**: {if tracked}

## Notes

{Any additional observations, questions, or concerns}

---

**Analysis Completed**: {date}  
**Review Status**: {Pending/Reviewed/Approved}  
**Next Steps**: {What should happen next}
