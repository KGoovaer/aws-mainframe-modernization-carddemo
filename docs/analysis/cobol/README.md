# COBOL Analysis Documentation

This directory contains systematic file-by-file analysis of all COBOL-related files in the CardDemo application, performed by the COBOL Analyst agent.

## Purpose

The COBOL Analysis phase is the **foundation** of the modernization effort. Before we can design modern architecture or write specifications, we need a complete understanding of:

1. **What each COBOL file does** - Business function and purpose
2. **How it works** - Technical implementation and logic
3. **What data it uses** - Data structures and dependencies
4. **How files relate** - Calls, data sharing, and integration points

## Directory Structure

```
cobol/
├── programs/          # Individual COBOL program analysis
│   ├── _TEMPLATE.md
│   ├── PROG-COSGN00C.md
│   ├── PROG-CBACT01C.md
│   └── ...
├── copybooks/         # Copybook and data structure analysis
│   ├── _TEMPLATE.md
│   ├── COPY-COCOM01Y.md
│   ├── COPY-CVACT01Y.md
│   └── ...
├── screens/           # BMS screen definition analysis
│   ├── SCREEN-COSGN00.md
│   ├── SCREEN-COMEN01.md
│   └── ...
├── jobs/              # JCL batch job analysis
│   ├── JOB-CBACT04.md
│   └── ...
└── summary/           # Cross-cutting analysis
    ├── module-map.md
    └── data-dictionary.md
```

## File Naming Convention

- **Programs**: `PROG-{PROGRAM-NAME}.md` (e.g., `PROG-COSGN00C.md`)
- **Copybooks**: `COPY-{COPYBOOK-NAME}.md` (e.g., `COPY-COCOM01Y.md`)
- **Screens**: `SCREEN-{SCREEN-NAME}.md` (e.g., `SCREEN-COSGN00.md`)
- **Jobs**: `JOB-{JOB-NAME}.md` (e.g., `JOB-CBACT04.md`)

Use UPPERCASE for names to match COBOL naming conventions.

## Analysis Scope

### Programs (app/cbl/)
Analysis of ~30 COBOL programs including:
- **Online Programs**: CICS transaction programs for user interaction
- **Batch Programs**: Batch processing and file maintenance
- **Utility Programs**: Common utilities and date functions

### Copybooks (app/cpy/)
Analysis of ~30 copybooks including:
- **Communication Areas**: CICS communication structures (COCOM01Y, etc.)
- **Entity Definitions**: Data records for accounts, cards, transactions, etc.
- **Utility Copybooks**: Common data structures and constants
- **Screen Maps**: BMS-generated copybooks

### Screens (app/bms/)
Analysis of ~17 BMS screen definitions for:
- User sign-on and menus
- Account management screens
- Card management screens
- Transaction inquiry screens
- User administration screens

### Jobs (app/jcl/)
Analysis of JCL jobs for:
- Daily batch processing
- Account interest calculation
- Transaction posting
- Statement generation
- Data import/export utilities

## Analysis Process

The COBOL Analyst follows this systematic process:

1. **Catalog Files**: Create inventory of all files to analyze
2. **Analyze Foundation**: Start with copybooks (data structures)
3. **Analyze Programs**: Systematically analyze programs
4. **Analyze Screens**: Document BMS screen definitions
5. **Analyze Jobs**: Document batch processing workflows
6. **Synthesize**: Create module map and data dictionary
7. **Track Progress**: Update `cobol-analysis-tracker.md` after each file

## How to Use These Documents

### For Architecture Analyst
- Read program and copybook analysis to understand business capabilities
- Use module groupings to identify microservice boundaries
- Reference data structures when defining entities
- Use call relationships to map integration points

### For Detailed Analyst
- Reference program analysis for detailed specification writing
- Use copybook analysis for data model definitions
- Check screen analysis for UI requirements
- Review job analysis for batch processing specs

### For Architect
- Use module map to design microservices architecture
- Reference data dictionary for database design
- Review integration points for API design
- Check batch jobs for async processing patterns

### For Developer
- Use program analysis as implementation reference
- Map copybook structures to C# classes
- Check business logic for validation rules
- Reference COBOL line numbers for traceability

## Key Artifacts

### Programs Directory
Contains detailed analysis of each COBOL program:
- Program structure and flow
- Business logic and calculations
- Data dependencies
- Integration points
- Modernization recommendations

### Copybooks Directory
Contains detailed analysis of each copybook:
- Complete field inventory
- Data type definitions
- Validation rules
- Redefines and occurs clauses
- Suggested .NET mappings

### Screens Directory
Contains analysis of BMS screens:
- Screen layout and fields
- User interaction flow
- Function key mappings
- Modern UI considerations

### Jobs Directory
Contains analysis of batch jobs:
- Job step breakdown
- Program execution flow
- Input/output files
- Scheduling requirements

### Summary Directory
Contains synthesized analysis:
- **module-map.md**: Logical grouping of programs into modules
- **data-dictionary.md**: Complete data dictionary from all copybooks

## Progress Tracking

Track analysis progress in: `../../state/cobol-analysis-tracker.md`

This file maintains:
- List of all files to analyze
- Current status of each file
- Recommended analysis order
- Priority assignments
- Module groupings
- Progress statistics

## Templates

Use the provided templates for consistency:
- `programs/_TEMPLATE.md` - Program analysis template
- `copybooks/_TEMPLATE.md` - Copybook analysis template

Templates ensure all analyses include:
- Comprehensive documentation
- Consistent structure
- COBOL line number references
- Modernization considerations
- Traceability to source code

## Analysis Standards

Each analysis document must include:

1. **Overview**: Clear description of purpose and function
2. **Structure**: Detailed breakdown of organization
3. **Logic**: Key business logic and calculations
4. **Dependencies**: Related files and programs
5. **Data**: Data structures and field definitions
6. **Line References**: COBOL source line numbers
7. **Modernization**: Recommendations for .NET implementation

## Integration with Workflow

This COBOL Analysis phase is **Step 1** in the modernization workflow:

```
1. COBOL Analyst → Analyzes all COBOL files
                 ↓
2. Architecture Analyst → Creates use cases from COBOL analysis
                        ↓
3. Detailed Analyst → Writes detailed specifications
                    ↓
4. Architect → Designs modern architecture
             ↓
5. Developer → Implements .NET code
             ↓
6. Test Manager → Tests implementation
```

The COBOL Analysis outputs are used by all subsequent agents.

## Quality Criteria

A complete COBOL analysis includes:

- ✅ All programs documented
- ✅ All copybooks documented
- ✅ All screens documented
- ✅ All batch jobs documented
- ✅ Module map created
- ✅ Data dictionary compiled
- ✅ All files marked complete in tracker
- ✅ Line numbers referenced for traceability
- ✅ Modernization considerations included

## Getting Started

To begin COBOL analysis:

1. Read `../../state/cobol-analysis-tracker.md`
2. Start with highest priority unanalyzed file
3. Use appropriate template
4. Analyze file thoroughly
5. Write markdown document
6. Update tracker
7. Move to next file

## Notes

- This is **documentation only** - no code is generated in this phase
- Focus on **understanding** before modernizing
- Be **thorough** - this analysis supports all future work
- Include **line numbers** for traceability
- Update **tracker** after each file
- **Systematic** approach ensures nothing is missed

---

For questions about COBOL analysis, refer to:
- `.github/agents/cobol-analyst.md` - Agent configuration
- `../../state/cobol-analysis-tracker.md` - Progress tracking
- Templates in this directory - Analysis structure
