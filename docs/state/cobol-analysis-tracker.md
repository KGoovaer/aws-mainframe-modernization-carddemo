# COBOL Analysis Tracker

This file tracks the systematic analysis of all COBOL-related files in the CardDemo application.

**Last Updated**: 2025-11-19  
**Analysis Phase**: Not Started  
**Overall Progress**: 0%

## Status Legend

- ⏳ Not Started
- 🔄 In Progress
- ✅ Complete
- ⚠️ Blocked
- 📝 Needs Review

## Summary Statistics

| Category | Total Files | Analyzed | In Progress | Not Started | Progress % |
|----------|-------------|----------|-------------|-------------|------------|
| Programs (cbl/) | 0 | 0 | 0 | 0 | 0% |
| Copybooks (cpy/) | 0 | 0 | 0 | 0 | 0% |
| Screens (bms/) | 0 | 0 | 0 | 0 | 0% |
| Jobs (jcl/) | 0 | 0 | 0 | 0 | 0% |
| **TOTAL** | **0** | **0** | **0** | **0** | **0%** |

---

## Programs (app/cbl/)

### Online Transaction Programs

| Program | Business Function | Status | Document | Analyzed Date | Module | Priority | Dependencies |
|---------|-------------------|--------|----------|---------------|--------|----------|--------------|
| COSGN00C | User Sign-on/Authentication | ⏳ Not Started | - | - | Authentication | High | COCOM01Y |
| COMEN01C | Main Menu | ⏳ Not Started | - | - | Menu | High | COCOM01Y |
| COADM01C | Admin Menu | ⏳ Not Started | - | - | Administration | Medium | COCOM01Y |
| COCRDLIC | Card List Inquiry | ⏳ Not Started | - | - | Card Management | Medium | CVCRD01Y |
| COCRDSLC | Card Select/Detail | ⏳ Not Started | - | - | Card Management | Medium | CVCRD01Y |
| COCRDUPC | Card Update | ⏳ Not Started | - | - | Card Management | Medium | CVCRD01Y |
| COACTVWC | Account View | ⏳ Not Started | - | - | Account Management | High | CVACT01Y |
| COACTUPC | Account Update | ⏳ Not Started | - | - | Account Management | High | CVACT01Y |
| COTRN00C | Transaction Menu | ⏳ Not Started | - | - | Transaction | High | COCOM01Y |
| COTRN01C | Transaction List | ⏳ Not Started | - | - | Transaction | High | CVTRA01Y |
| COTRN02C | Transaction Detail | ⏳ Not Started | - | - | Transaction | High | CVTRA01Y |
| COUSR00C | User List | ⏳ Not Started | - | - | User Management | Medium | CSUSR01Y |
| COUSR01C | User Add | ⏳ Not Started | - | - | User Management | Medium | CSUSR01Y |
| COUSR02C | User Update | ⏳ Not Started | - | - | User Management | Medium | CSUSR01Y |
| COUSR03C | User Delete | ⏳ Not Started | - | - | User Management | Medium | CSUSR01Y |
| CORPT00C | Reports Menu | ⏳ Not Started | - | - | Reporting | Low | COCOM01Y |
| COBIL00C | Billing | ⏳ Not Started | - | - | Reporting | Low | - |

### Batch Programs

| Program | Business Function | Status | Document | Analyzed Date | Module | Priority | Dependencies |
|---------|-------------------|--------|----------|---------------|--------|----------|--------------|
| CBACT01C | Account File Browse | ⏳ Not Started | - | - | Account Batch | Medium | CVACT01Y |
| CBACT02C | Account File Update | ⏳ Not Started | - | - | Account Batch | Medium | CVACT01Y |
| CBACT03C | Account Cross-Reference | ⏳ Not Started | - | - | Account Batch | Medium | CVACT02Y, CVACT03Y |
| CBACT04C | Account Interest Calculation | ⏳ Not Started | - | - | Account Batch | High | CVACT01Y |
| CBCUS01C | Customer File Update | ⏳ Not Started | - | - | Customer Batch | Medium | CVCUS01Y |
| CBTRN01C | Transaction File Browse | ⏳ Not Started | - | - | Transaction Batch | High | CVTRA05Y |
| CBTRN02C | Transaction Posting | ⏳ Not Started | - | - | Transaction Batch | High | CVTRA01Y-05Y |
| CBTRN03C | Transaction Category Balance | ⏳ Not Started | - | - | Transaction Batch | Medium | CVTRA04Y |
| CBSTM03A | Statement File Read | ⏳ Not Started | - | - | Statement Batch | Medium | COSTM01 |
| CBSTM03B | Statement Print | ⏳ Not Started | - | - | Statement Batch | Medium | COSTM01 |
| CBIMPORT | Data Import Utility | ⏳ Not Started | - | - | Utility | Low | CVEXPORT |
| CBEXPORT | Data Export Utility | ⏳ Not Started | - | - | Utility | Low | CVEXPORT |

### Utility Programs

| Program | Business Function | Status | Document | Analyzed Date | Module | Priority | Dependencies |
|---------|-------------------|--------|----------|---------------|--------|----------|--------------|
| CSUTLDTC | Date/Time Utilities | ⏳ Not Started | - | - | Utilities | Medium | CSUTLDPY, CSUTLDWY |
| COBSWAIT | Wait/Delay Function | ⏳ Not Started | - | - | Utilities | Low | - |

---

## Copybooks (app/cpy/)

### Communication Areas

| Copybook | Purpose | Status | Document | Analyzed Date | Used By | Priority |
|----------|---------|--------|----------|---------------|---------|----------|
| COCOM01Y | Common Communication Area | ⏳ Not Started | - | - | All Online Programs | High |
| COADM02Y | Admin Communication Area | ⏳ Not Started | - | - | COADM01C | Medium |
| COMEN02Y | Menu Communication Area | ⏳ Not Started | - | - | COMEN01C | High |

### Entity Definitions

| Copybook | Purpose | Status | Document | Analyzed Date | Used By | Priority |
|----------|---------|--------|----------|---------------|---------|----------|
| CUSTREC | Customer Record | ⏳ Not Started | - | - | Customer programs | High |
| CVACT01Y | Account Record | ⏳ Not Started | - | - | Account programs | High |
| CVACT02Y | Account Cross-Reference | ⏳ Not Started | - | - | CBACT03C | Medium |
| CVACT03Y | Account Additional Data | ⏳ Not Started | - | - | CBACT03C | Medium |
| CVCRD01Y | Card Record | ⏳ Not Started | - | - | Card programs | High |
| CVCUS01Y | Customer Update Record | ⏳ Not Started | - | - | CBCUS01C | Medium |
| CVTRA01Y | Transaction Record | ⏳ Not Started | - | - | Transaction programs | High |
| CVTRA02Y | Transaction Summary | ⏳ Not Started | - | - | Transaction programs | High |
| CVTRA03Y | Transaction Detail | ⏳ Not Started | - | - | Transaction programs | High |
| CVTRA04Y | Transaction Category | ⏳ Not Started | - | - | CBTRN03C | Medium |
| CVTRA05Y | Transaction File Layout | ⏳ Not Started | - | - | CBTRN01C | Medium |

### Screen Map Copybooks

| Copybook | Purpose | Status | Document | Analyzed Date | Screen | Priority |
|----------|---------|--------|----------|---------------|--------|----------|
| COSGN00 | Sign-on Screen Map | ⏳ Not Started | - | - | COSGN00 | High |
| COMEN01 | Main Menu Screen Map | ⏳ Not Started | - | - | COMEN01 | High |
| COADM01 | Admin Menu Screen Map | ⏳ Not Started | - | - | COADM01 | Medium |

### Utility/Common Copybooks

| Copybook | Purpose | Status | Document | Analyzed Date | Used By | Priority |
|----------|---------|--------|----------|---------------|---------|----------|
| CSDAT01Y | Date Data Structures | ⏳ Not Started | - | - | Date processing programs | Medium |
| CSMSG01Y | Message Definitions | ⏳ Not Started | - | - | All programs | High |
| CSMSG02Y | Extended Messages | ⏳ Not Started | - | - | All programs | Medium |
| CSSETATY | SET Attribute | ⏳ Not Started | - | - | Screen programs | Low |
| CSSTRPFY | String Processing | ⏳ Not Started | - | - | Various programs | Low |
| CSLKPCDY | Lookup Code | ⏳ Not Started | - | - | Various programs | Low |
| CSUSR01Y | User Data Structure | ⏳ Not Started | - | - | User programs | Medium |
| CSUTLDPY | Date Utility Parameters | ⏳ Not Started | - | - | CSUTLDTC | Medium |
| CSUTLDWY | Date Utility Work Areas | ⏳ Not Started | - | - | CSUTLDTC | Medium |
| COTTL01Y | Title/Header Definitions | ⏳ Not Started | - | - | Report programs | Low |
| CVEXPORT | Export/Import Layout | ⏳ Not Started | - | - | CBIMPORT, CBEXPORT | Low |
| COSTM01 | Statement Record | ⏳ Not Started | - | - | CBSTM03A, CBSTM03B | Medium |
| CODATECN | Date Conversion | ⏳ Not Started | - | - | Date programs | Medium |

---

## Screens (app/bms/)

| Screen | Program | Purpose | Status | Document | Analyzed Date | Priority |
|--------|---------|---------|--------|----------|---------------|----------|
| COSGN00 | COSGN00C | User Sign-on | ⏳ Not Started | - | - | High |
| COMEN01 | COMEN01C | Main Menu | ⏳ Not Started | - | - | High |
| COADM01 | COADM01C | Admin Menu | ⏳ Not Started | - | - | Medium |
| COCRDLI | COCRDLIC | Card List | ⏳ Not Started | - | - | Medium |
| COCRDSL | COCRDSLC | Card Select | ⏳ Not Started | - | - | Medium |
| COCRDUP | COCRDUPC | Card Update | ⏳ Not Started | - | - | Medium |
| COACTVW | COACTVWC | Account View | ⏳ Not Started | - | - | High |
| COACTUP | COACTUPC | Account Update | ⏳ Not Started | - | - | High |
| COTRN00 | COTRN00C | Transaction Menu | ⏳ Not Started | - | - | High |
| COTRN01 | COTRN01C | Transaction List | ⏳ Not Started | - | - | High |
| COTRN02 | COTRN02C | Transaction Detail | ⏳ Not Started | - | - | High |
| COUSR00 | COUSR00C | User List | ⏳ Not Started | - | - | Medium |
| COUSR01 | COUSR01C | User Add | ⏳ Not Started | - | - | Medium |
| COUSR02 | COUSR02C | User Update | ⏳ Not Started | - | - | Medium |
| COUSR03 | COUSR03C | User Delete | ⏳ Not Started | - | - | Medium |
| CORPT00 | CORPT00C | Reports Menu | ⏳ Not Started | - | - | Low |
| COBIL00 | COBIL00C | Billing Screen | ⏳ Not Started | - | - | Low |

---

## Batch Jobs (app/jcl/)

| Job | Programs | Purpose | Status | Document | Analyzed Date | Priority | Frequency |
|-----|----------|---------|--------|----------|---------------|----------|-----------|
| - | - | (To be cataloged) | ⏳ Not Started | - | - | - | - |

---

## Analysis Progress by Module

| Module | Programs | Analyzed | Progress % | Status |
|--------|----------|----------|------------|--------|
| Authentication | 1 | 0 | 0% | ⏳ Not Started |
| Menu | 2 | 0 | 0% | ⏳ Not Started |
| Account Management | 6 | 0 | 0% | ⏳ Not Started |
| Card Management | 3 | 0 | 0% | ⏳ Not Started |
| Transaction | 6 | 0 | 0% | ⏳ Not Started |
| User Management | 4 | 0 | 0% | ⏳ Not Started |
| Reporting | 4 | 0 | 0% | ⏳ Not Started |
| Utilities | 4 | 0 | 0% | ⏳ Not Started |

---

## Recommended Analysis Order

### Phase 1: Foundation (Copybooks & Utilities)
Priority: **High** - Provides foundation for understanding all programs

1. ✅ COCOM01Y - Common communication area (used by all)
2. ✅ CSMSG01Y - Message definitions
3. ✅ CSDAT01Y - Date structures
4. ✅ CUSTREC - Customer record
5. ✅ CVACT01Y - Account record
6. ✅ CVCRD01Y - Card record
7. ✅ CVTRA01Y - Transaction record
8. ✅ CSUTLDTC - Date utilities program

### Phase 2: Core Online Programs
Priority: **High** - Main user-facing functionality

9. ✅ COSGN00C + COSGN00 screen - Authentication entry point
10. ✅ COMEN01C + COMEN01 screen - Main menu
11. ✅ COACTVWC + COACTVW screen - Account viewing
12. ✅ COTRN00C + COTRN00 screen - Transaction menu
13. ✅ COTRN01C + COTRN01 screen - Transaction list
14. ✅ COTRN02C + COTRN02 screen - Transaction detail

### Phase 3: Critical Batch Programs
Priority: **High** - Core business processing

15. ✅ CBTRN02C - Transaction posting (critical)
16. ✅ CBACT04C - Interest calculation
17. ✅ CBACT01C - Account file browse

### Phase 4: Extended Online Programs
Priority: **Medium** - Additional online features

18. ✅ COCRDLIC + COCRDLI screen - Card list
19. ✅ COCRDSLC + COCRDSL screen - Card select
20. ✅ COACTUPC + COACTUP screen - Account update
21. ✅ COUSR00C-03C + screens - User management suite

### Phase 5: Reporting & Admin
Priority: **Medium** - Secondary features

22. ✅ CBSTM03A, CBSTM03B - Statement generation
23. ✅ CORPT00C + CORPT00 screen - Reports
24. ✅ COADM01C + COADM01 screen - Admin menu
25. ✅ COBIL00C + COBIL00 screen - Billing

### Phase 6: Remaining Batch & Utilities
Priority: **Low** - Supporting functions

26. ✅ CBACT02C, CBACT03C - Account batch utilities
27. ✅ CBCUS01C - Customer update
28. ✅ CBTRN01C, CBTRN03C - Transaction utilities
29. ✅ CBIMPORT, CBEXPORT - Import/export utilities
30. ✅ Remaining copybooks and batch jobs

---

## Current Focus

**Status**: Not Started  
**Current File**: None  
**Next File**: COCOM01Y (Common communication area copybook)

---

## Blockers

None at this time.

---

## Notes

- Analysis order prioritizes foundation (copybooks) before programs
- Core business flows (authentication, accounts, transactions) analyzed first
- Batch processing analysis follows online programs
- Utility and admin functions analyzed last
- Each file must have complete documentation before marking as complete
- Update this tracker immediately after completing each file analysis

---

## Change Log

| Date | File | Change | Analyst |
|------|------|--------|---------|
| 2025-11-19 | - | Initial tracker created | System |

