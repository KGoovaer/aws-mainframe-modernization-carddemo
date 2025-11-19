# COBOL Codebase Overview

This document provides a high-level overview of the CardDemo COBOL application structure under `app/`.

## Directory Structure

### Core Components

**Program Files:**
- **`cbl/`** - 35+ COBOL program source files (online CICS + batch)
- **`cpy/`** - 30+ copybooks (shared data structures and common code)
- **`cpy-bms/`** - BMS-generated copybooks for screen I/O
- **`bms/`** - 17 BMS map definitions (screen layouts)
- **`jcl/`** - 38 Job Control Language files for batch execution
- **`data/`** - Sample VSAM data files (ASCII and EBCDIC subdirectories)

**Supporting Files:**
- **`asm/`** - Assembler utilities (COBDATFT, MVSWAIT)
- **`catlg/`** - VSAM catalog listings
- **`csd/`** - CICS System Definition (resource definitions)
- **`ctl/`** - Control files
- **`maclib/`** - Macro libraries
- **`proc/`** - JCL procedures
- **`scheduler/`** - Scheduling definitions

### Extension Modules (Separate Subdirectories)

- **`app-authorization-ims-db2-mq/`** - Credit card authorization with IMS, DB2, MQ
- **`app-transaction-type-db2/`** - Transaction type management with DB2
- **`app-vsam-mq/`** - Account extraction via MQ

## Program Types & Naming Conventions

### Naming Pattern

Programs follow the pattern: **`[Type][Function][Sequence][Suffix]`**

**Online/CICS Programs (CO*):**
- Pattern: `CO[function][sequence]C.cbl`
- Examples:
  - `COSGN00C` - Signon screen
  - `COMEN01C` - Main menu
  - `COCRDLIC` - Credit card list
  - `COADM01C` - Admin menu
  - `COTRN00C` - Transaction list
  - `COBIL00C` - Bill payment
  - `COACTVWC` - Account view
  - `COACTUPC` - Account update

**Batch Programs (CB*):**
- Pattern: `CB[function][sequence]C.cbl`
- Examples:
  - `CBACT01C` - Account batch processing
  - `CBTRN02C` - Transaction posting (main daily batch)
  - `CBACT04C` - Interest calculation
  - `CBSTM03A` - Statement generation
  - `CBCUS01C` - Customer data processing
  - `CBIMPORT` - Data import utility
  - `CBEXPORT` - Data export utility

**Utility Programs (CS*):**
- `CSUTLDTC` - Date utility
- `COBSWAIT` - Wait utility

### Program Categories

**1. Online Programs (CICS):**
- **User Management:** COUSR00C-03C (list, add, update, delete)
- **Account Management:** COACTVWC, COACTUPC (view, update)
- **Card Management:** COCRDLIC, COCRDSLC, COCRDUPC (list, view, update)
- **Transaction Management:** COTRN00C-02C (list, view, add)
- **Reporting:** CORPT00C (generate reports)
- **Bill Payment:** COBIL00C
- **Administration:** COADM01C, COSGN00C, COMEN01C

**2. Batch Programs:**
- **Transaction Processing:** CBTRN02C (daily posting)
- **Account Processing:** CBACT01C-04C (various account operations)
- **Customer Processing:** CBCUS01C
- **Statement Generation:** CBSTM03A, CBSTM03B
- **Interest Calculation:** CBACT04C
- **Reporting:** CBTRN03C
- **Import/Export:** CBIMPORT, CBEXPORT

## Data Dependencies

### VSAM Files (Primary Storage)

| File | DD Name | Key | Record Length | Purpose | Copybook |
|------|---------|-----|---------------|---------|----------|
| Account Data | ACCTDATA | 11-byte account ID | 300 | Account master | CVACT01Y |
| Card Data | CARDDATA | 16-byte card number | 150 | Card master | CVACT02Y |
| Customer Data | CUSTDATA | 9-byte customer ID | 500 | Customer master | CVCUS01Y |
| Card Cross-Reference | CARDXREF | 16-byte card number | 50 | Account-Card-Customer link | CVACT03Y |
| Transaction Data | TRANSACT | 16-byte transaction ID | 350 | Transaction master | CVTRA05Y |
| Transaction Category Balance | TCATBALF | 17-byte composite key | 50 | Category balances | CVTRA01Y |
| User Security | USRSEC | 8-byte user ID | 80 | User authentication | CSUSR01Y |
| Disclosure Group | DISCGRP | 16-byte | 50 | Disclosure groups | CVTRA02Y |
| Transaction Category | TRANCATG | 6-byte | 60 | Category types | CVTRA04Y |
| Transaction Type | TRANTYPE | 2-byte | 60 | Transaction types | CVTRA03Y |

**File Characteristics:**
- All are KSDS (Key-Sequenced Data Sets)
- Some have Alternate Indexes (AIX) for secondary access paths
- SHAREOPTIONS(2,3) for CICS multi-region access
- ERASE attribute for security-sensitive data

### Sequential Files

- **DALYTRAN.PS** - Daily transaction input (CVTRA06Y)
- **DALYREJS** - Rejected transactions
- **Export/Import data** - Various formats

### GDG (Generation Data Groups)

- **TRANSACT.BKUP** - Transaction backups
- **SYSTRAN** - System transactions
- **DALYREJS** - Rejected transaction history
- **TRANREPT** - Transaction reports
- **TCATBALF.BKUP** - Balance backups

### Copybook Categories

**Data Structures (CV*):**
- `CVACT01Y` - Account record (300 bytes)
- `CVACT02Y` - Card record (150 bytes)
- `CVACT03Y` - Cross-reference (50 bytes)
- `CVCUS01Y` - Customer record (500 bytes)
- `CVTRA01Y-07Y` - Various transaction structures

**Common Areas (CO*, CS*):**
- `COCOM01Y` - Communication area (COMMAREA) - **Critical for program-to-program navigation**
- `CSMSG01Y` - Common messages
- `COTTL01Y` - Title information
- `CSDAT01Y` - Date handling

**Screen/BMS (Map copybooks):**
- Generated from BMS maps (e.g., COSGN00, COCRDLI)
- Define input/output field layouts

## Program Relationships & Patterns

### Screen Flow Navigation

**Uses COMMAREA (`COCOM01Y`) for context passing:**

```cobol
01 CARDDEMO-COMMAREA.
   05 CDEMO-GENERAL-INFO.
      10 CDEMO-FROM-TRANID         - Calling transaction ID
      10 CDEMO-FROM-PROGRAM        - Calling program name
      10 CDEMO-TO-TRANID           - Target transaction ID
      10 CDEMO-TO-PROGRAM          - Target program name
      10 CDEMO-USER-ID             - Current user ID
      10 CDEMO-USER-TYPE           - Admin/Regular user
      10 CDEMO-PGM-CONTEXT         - First entry/Reentry flag
   05 CDEMO-CUSTOMER-INFO.         - Customer context data
   05 CDEMO-ACCOUNT-INFO.          - Account context data
   05 CDEMO-CARD-INFO.             - Card context data
```

**Typical Navigation Flow:**

1. **COSGN00C (CC00)** → User login
2. **COMEN01C (CM00)** → Main menu (regular user) OR **COADM01C (CA00)** → Admin menu
3. Branch to specific transactions:
   - CAVW/CAUP - Account view/update
   - CCLI/CCDL/CCUP - Card list/view/update
   - CT00/CT01/CT02 - Transaction list/view/add
   - CR00 - Reports
   - CB00 - Bill payment
   - CU00-CU03 - User management (admin only)

**Transfer Methods:**
- **EXEC CICS XCTL** - Transfer control with COMMAREA
- **EXEC CICS RETURN** - Return to calling program or await next user input
- **CALL** - For utilities (CSUTLDTC, COBDATFT, MVSWAIT)

### Data Access Patterns

**Online Programs (CICS):**

```cobol
EXEC CICS READ
     DATASET('ACCTDAT')
     INTO(ACCOUNT-RECORD)
     RIDFLD(ACCT-ID)
     KEYLENGTH(LENGTH OF ACCT-ID)
     RESP(WS-RESP-CD)
     RESP2(WS-REAS-CD)
END-EXEC
```

**Batch Programs:**

```cobol
SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
    ORGANIZATION IS INDEXED
    ACCESS MODE IS RANDOM
    RECORD KEY IS ACCT-ID
    FILE STATUS IS ACCT-FILE-STATUS.
```

**Common Processing Pattern:**
1. Open files
2. Process records (sequential or random access)
3. Update related files (Account → Transaction → Category Balance)
4. Close files

### Business Logic Flow Example

**Transaction Posting (CBTRN02C):**

1. Read daily transaction file (DALYTRAN)
2. Validate transaction via cross-reference (CARDXREF)
3. Write to transaction file (TRANSACT)
4. Update account balance (ACCTDAT)
5. Update category balance (TCATBALF)
6. Write rejects to DALYREJS

## Special Conventions & Patterns

### Naming Conventions

**Programs:**
- First 2 chars: CO (online), CB (batch), CS (utility)
- Next 3-4 chars: Function abbreviation
- Last 2 chars: Sequence number
- Suffix: C (COBOL program)

**Copybooks:**
- CV* - VSAM record structures
- CO* - Common areas, screen structures
- CS* - System utilities, messages
- Suffix: Y for copybook

**BMS Maps:**
- Same base name as program (e.g., COSGN00 for COSGN00C)
- Generate copybooks in cpy-bms/

**Transaction IDs (CICS):**
- 4-character IDs
- CC00 - Signon
- CM00 - Main menu
- CA00 - Admin menu
- C[function][sequence] - Specific functions

### Data Structure Patterns

**COMP Fields:**
- Response codes, reason codes
- Binary counters and indexes

**COMP-3 (Packed Decimal):**
- Monetary amounts, balances
- Interest rate calculations

**OCCURS/Arrays:**
- Statement lines (2D arrays in CBSTM03A)
- Screen line items for lists

**REDEFINES:**
- Alternative views of the same data
- Type conversions and formatting

### Error Handling

**Online (CICS):**

```cobol
EXEC CICS READ ... 
     RESP(WS-RESP-CD)
     RESP2(WS-REAS-CD)
END-EXEC

EVALUATE WS-RESP-CD
    WHEN DFHRESP(NORMAL)
        CONTINUE
    WHEN DFHRESP(NOTFND)
        ... handle not found ...
    WHEN OTHER
        ... handle error ...
END-EVALUATE
```

**Batch:**

```cobol
FILE STATUS checking:
- 00 = Successful completion
- 10 = End of file
- 23 = Record not found
- etc.
```

## Key Integration Points

### Optional Extension Integration

**1. IMS-DB2-MQ Authorization:**
- Programs: COPAUA0C, COPAUS0C-2C, CBPAUP0C
- Demonstrates: MQ messaging, IMS hierarchical DB, DB2 relational access
- Integrates with main app via VSAM cross-reference

**2. DB2 Transaction Type:**
- Programs: COTRTLIC, COTRTUPC, COBTUPDT
- Demonstrates: Static embedded SQL, cursors, CRUD operations
- Extracts to VSAM for runtime use (TRANEXTR job)

**3. MQ Account Inquiry:**
- Programs: CODATE01, COACCT01
- Demonstrates: Request/response patterns via MQ

### Technology Stack

**Core Technologies:**
- COBOL (procedural, structured programming)
- CICS (transaction processing, pseudo-conversational)
- VSAM (indexed, sequential file access)
- JCL (batch job orchestration)
- BMS (Basic Mapping Support for screen definitions)

**Optional Technologies:**
- DB2 (relational SQL database)
- IMS DB (hierarchical database)
- IBM MQ (messaging middleware)

## Critical Concepts for Understanding Behavior

### 1. COMMAREA Structure (`COCOM01Y`)
**Most Important Concept** - Maintains all session state between screen interactions. Contains:
- User context (ID, type, authentication)
- Navigation history (from/to programs)
- Business context (customer, account, card IDs)
- Screen-specific data

### 2. VSAM File Relationships
Data model linkage:
- **CARDXREF** is the central hub linking Card → Account → Customer
- **ACCTDAT** maintains account balances
- **TRANSACT** stores all transaction history
- **TCATBALF** aggregates balances by category

### 3. Copybook-Driven Design
- All data structures defined in copybooks
- Programs copy identical definitions for consistency
- Changes to copybooks require recompilation of all dependent programs

### 4. Pseudo-Conversational CICS Pattern
Each user interaction:
1. RECEIVE map (read user input)
2. Process business logic
3. SEND map (display results)
4. RETURN to CICS (release resources, await next input)

State persists **only** in COMMAREA between interactions.

### 5. Multi-File Update Patterns
Business transactions typically update multiple related files:
- Payment: Update TRANSACT + ACCTDAT balance + TCATBALF
- Card issue: Update CARDDATA + CARDXREF + possibly ACCTDAT
- Referential integrity maintained by program logic (no database constraints)

### 6. Error Recovery
- Limited transaction logging
- Manual recovery patterns
- VSAM backups via GDGs
- Rejected transaction files for batch processing

## Documentation Resources

- **`README.md`** (root) - Comprehensive installation, configuration, feature overview
- **Extension READMEs** - Detailed setup for optional modules
- **`catlg/LISTCAT.txt`** - Complete VSAM catalog (file definitions, statistics)
- **Inline comments** - Programs contain headers with purpose, copyright
- **BMS source** - Screen layouts with field attributes documented
- **JCL** - Self-documenting with step descriptions

## Architecture Patterns

This codebase follows **classic mainframe patterns**:

1. **Separation of Concerns**: Clear division between online (CICS) and batch processing
2. **Structured Data Access**: VSAM indexed files for efficient keyed access
3. **Pseudo-Conversational Design**: Resource-efficient CICS interaction model
4. **Tight Coupling via Copybooks**: Shared data definitions ensure consistency
5. **Program-Driven Integrity**: Business rules and referential integrity enforced in COBOL code
6. **Stateless Transactions**: Each transaction is independent, state maintained in COMMAREA

## Modernization Considerations

When modernizing this codebase, key dependencies to understand:

1. **COMMAREA Pattern** - Maps to session management in modern architectures
2. **VSAM Relationships** - Need to implement referential integrity in target database
3. **Transaction Lifecycle** - From input (DALYTRAN) through multiple file updates
4. **Pseudo-Conversational State** - Modern stateless API patterns are analogous
5. **Multi-File Atomicity** - May need distributed transaction coordination
6. **Error Handling** - Legacy patterns need translation to modern exception handling
7. **Security Model** - RACF integration and USRSEC file-based authorization

