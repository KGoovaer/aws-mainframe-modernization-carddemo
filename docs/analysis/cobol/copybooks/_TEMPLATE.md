# Copybook Analysis: {COPYBOOK-NAME}

**Template Version**: 1.0  
**Last Updated**: {DATE}

## Overview
**Copybook Name**: {copybook name}  
**Source File**: `app/cpy/{filename}.cpy` or `app/cpy-bms/{filename}.cpy`  
**Type**: Data Structure/Communication Area/Constants/Screen Map  
**Purpose**: {What this copybook defines}  
**Category**: {Entity/Communication/Utility/Screen}  
**Analyzed By**: COBOL Analyst  
**Analysis Date**: {date}

## Purpose
{Detailed description of what this copybook defines and why it exists - 1-2 paragraphs}

## Usage Statistics
**Used By**: {Number} programs  
**Frequency**: {How often used - High/Medium/Low}  
**Critical**: {Yes/No - Is this foundational?}

## Data Structure

### Full Structure Diagram
```cobol
{Show the complete copybook structure with proper indentation}
{Include all level numbers, names, PICTURE clauses, and VALUE clauses}

01  {RECORD-NAME}.
    05  {FIELD-1}              PIC X(10).
    05  {FIELD-2}              PIC 9(5)V99.
    05  {GROUP-FIELD}.
        10  {SUB-FIELD-1}      PIC X(20).
        10  {SUB-FIELD-2}      PIC 9(8).
```

### Structure Summary
**Top-Level Record**: {01-level name}  
**Total Length**: {bytes}  
**Number of Fields**: {count}  
**Number of Groups**: {count}  
**Number of Elementary Items**: {count}

## Field Definitions

### Complete Field Inventory

| Level | Field Name | Picture | Type | Length | Decimals | Description | Notes |
|-------|------------|---------|------|--------|----------|-------------|-------|
| 01 | {RECORD} | - | Group | {total} | - | {Purpose} | Top level |
| 05 | {FIELD} | X(10) | Alphanumeric | 10 | - | {Purpose} | {Notes} |
| 05 | {FIELD} | 9(5)V99 | Numeric | 7 | 2 | {Purpose} | {Notes} |
| 05 | {GROUP} | - | Group | {bytes} | - | {Purpose} | {Notes} |
| 10 | {SUBFIELD} | X(20) | Alphanumeric | 20 | - | {Purpose} | {Notes} |

### Field Categories

#### Identification Fields
| Field | Purpose | Validation |
|-------|---------|------------|
| {FIELD} | {Purpose} | {Validation rules} |

#### Data Fields
| Field | Purpose | Validation |
|-------|---------|------------|
| {FIELD} | {Purpose} | {Validation rules} |

#### Control Fields
| Field | Purpose | Validation |
|-------|---------|------------|
| {FIELD} | {Purpose} | {Validation rules} |

#### Status/Flag Fields
| Field | Purpose | Valid Values |
|-------|---------|--------------|
| {FIELD} | {Purpose} | {Values} |

## Special COBOL Features

### Computed Fields
| Field | Computation | Purpose |
|-------|-------------|---------|
| {FIELD} | {Formula/Derivation} | {Purpose} |

### Redefines
| Field | Redefines | Purpose | Implications |
|-------|-----------|---------|--------------|
| {FIELD-1} | {FIELD-2} | {Purpose} | {Impact on data interpretation} |

**Redefine Details**:
```cobol
{Show the redefines structure}
05  {FIELD-1}           PIC X(20).
05  {FIELD-2} REDEFINES {FIELD-1}.
    10  {SUBFIELD-A}    PIC X(10).
    10  {SUBFIELD-B}    PIC X(10).
```

### Occurs Clauses
| Field | Occurs | Indexed By | Purpose |
|-------|--------|------------|---------|
| {FIELD} | {n} TIMES | {INDEX} | {Purpose - array/table} |

**Array Details**:
```cobol
{Show the occurs structure}
05  {TABLE-NAME} OCCURS {n} TIMES.
    10  {ELEMENT}       PIC {clause}.
```

### Value Clauses (Constants)
| Field | Value | Purpose |
|-------|-------|---------|
| {FIELD} | '{value}' | {Purpose - constant definition} |

### Condition Names (88-levels)
| Condition Name | Parent Field | Value | Meaning |
|----------------|--------------|-------|---------|
| {CONDITION} | {FIELD} | '{value}' | {What the condition represents} |

## Data Types and Formats

### Alphanumeric Fields
| Field | Length | Format | Example | Validation |
|-------|--------|--------|---------|------------|
| {FIELD} | {n} | {Format} | {Example} | {Rules} |

### Numeric Fields
| Field | Integer Digits | Decimal Digits | Signed | Range | Validation |
|-------|----------------|----------------|--------|-------|------------|
| {FIELD} | {n} | {d} | Yes/No | {min-max} | {Rules} |

### Date/Time Fields
| Field | Format | Example | Notes |
|-------|--------|---------|-------|
| {FIELD} | {Format} | {Example} | {Interpretation} |

### Packed Decimal Fields
| Field | Digits | Decimals | Usage | Purpose |
|-------|--------|----------|-------|---------|
| {FIELD} | {n} | {d} | COMP-3 | {Purpose} |

## Validation Rules

### Field-Level Validations
| Field | Rule | Error Condition | Example |
|-------|------|-----------------|---------|
| {FIELD} | {Rule description} | {What triggers error} | {Example} |

### Business Rules
1. **{RULE-NAME}**: {Description of cross-field validation or business rule}
2. **{RULE-NAME}**: {Description}

### Data Integrity Constraints
- **Mandatory Fields**: {List fields that cannot be spaces/zeros}
- **Referential Integrity**: {Relationships to other records}
- **Format Requirements**: {Special format rules}

## Relationships

### Used In Programs
| Program | Usage Context | Access Pattern | Frequency |
|---------|---------------|----------------|-----------|
| {PROGRAM} | {How it's used} | Read/Write/Both | High/Med/Low |

### Related Copybooks
| Copybook | Relationship | Shared Fields | Purpose |
|----------|--------------|---------------|---------|
| {COPYBOOK} | {Parent/Child/Peer} | {Fields} | {Why related} |

### Related Files
| File | Relationship | Key Fields | Access |
|------|--------------|------------|--------|
| {FILE} | {Maps to this record} | {Keys} | {Type} |

## Data Flow

### Input Sources
- **{SOURCE}**: {How data enters this structure}

### Output Destinations  
- **{DESTINATION}**: {Where this data goes}

### Transformation Points
- **{PROGRAM}**: {How data is transformed}

## Sample Data

### Example Record
```
{Show a sample populated record with realistic values}
{Use proper spacing to show field positions}

Field Name         Value
-----------------  --------------------
{FIELD-1}          {sample value}
{FIELD-2}          {sample value}
```

### Test Data Scenarios
1. **Valid Record**: {Description with values}
2. **Edge Case 1**: {Description with values}
3. **Error Case 1**: {Description with values}

## Record Layout Diagram

```
Position  Length  Field Name              Type        Description
--------  ------  ----------------------  ----------  ---------------------------
{001-010} {10}    {FIELD-NAME}           {X(10)}     {Description}
{011-017} {7}     {FIELD-NAME}           {9(5)V99}   {Description}
{018-037} {20}    {FIELD-NAME}           {X(20)}     {Description}
```

**Total Record Length**: {bytes} bytes

## Technical Specifications

### Memory Layout
**Alignment**: {Byte alignment requirements}  
**Padding**: {Any padding considerations}  
**Endianness**: {If relevant for binary fields}

### Performance Considerations
- **Index Usage**: {If used with occurs}
- **Redefine Impact**: {Memory sharing implications}
- **Frequency of Access**: {High/Medium/Low}

## Modernization Mapping

### Target .NET Type
**Suggested Type**: {C# class/record/struct}  
**Pattern**: {DTO/Entity/ViewModel/etc.}

### Field Mapping
| COBOL Field | .NET Property | .NET Type | Conversion Notes |
|-------------|---------------|-----------|------------------|
| {FIELD} | {Property} | {Type} | {Conversion approach} |

### Mapping Considerations
1. **Data Type Conversions**: {Key conversions needed}
2. **Validation Migration**: {How to preserve validations}
3. **Business Rules**: {How to implement in .NET}
4. **Naming Conventions**: {How to modernize names}

### Sample C# Class
```csharp
// Suggested modern equivalent
public class {ClassName}
{
    {Properties with appropriate types}
    
    // Example:
    // public string FieldName { get; set; }
    // public decimal NumericField { get; set; }
}
```

### Entity Framework Considerations
- **Table Mapping**: {Suggested table name}
- **Key Fields**: {Primary/foreign keys}
- **Indexes**: {Recommended indexes}
- **Navigation Properties**: {Relationships}

## Historical Context

### Origin
**Created**: {Date if known}  
**Original Purpose**: {Why it was created}  
**Evolution**: {How it has changed over time}

### Modifications
- **{Date}**: {Change description}
- **{Date}**: {Change description}

## Quality Assessment

### Structural Quality
**Organization**: {Well/Poorly organized}  
**Naming**: {Clear/Unclear names}  
**Documentation**: {Well/Poorly documented in source}

### Design Patterns
- **Strengths**: {Good design aspects}
- **Concerns**: {Design issues}
- **Complexity**: {Simple/Moderate/Complex}

## Modernization Analysis

### Modernization Priority
**Priority**: {High/Medium/Low}  
**Rationale**: {Why this priority}

### Complexity Assessment
**Complexity**: {Simple/Moderate/Complex}  
**Key Challenges**:
1. {Challenge 1}
2. {Challenge 2}

### Recommended Approach
1. {Approach step 1}
2. {Approach step 2}

## Documentation References

### Related Analysis Documents
- **Programs**: {List of program analysis docs that use this}
- **Copybooks**: {List of related copybook analysis docs}
- **Screens**: {List of screen docs if applicable}

### Source Code Location
- **Repository Path**: `app/cpy/{filename}.cpy`
- **Version**: {if tracked}

## Cross-Reference

### Where Used
```
{List of all programs that COPY this copybook}
{Include line numbers if possible}

PROGRAM-1 (Line 123)
PROGRAM-2 (Line 456)
...
```

## Notes

{Any additional observations, questions, or concerns about this copybook}

### Open Questions
- {Question 1}
- {Question 2}

### Recommendations
- {Recommendation 1}
- {Recommendation 2}

---

**Analysis Completed**: {date}  
**Review Status**: {Pending/Reviewed/Approved}  
**Next Steps**: {What should happen next with this copybook}
