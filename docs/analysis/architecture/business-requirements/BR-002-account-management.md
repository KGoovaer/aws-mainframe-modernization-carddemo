# BR-002: Account Management

## Business Context
Account management is a core capability that enables users to view and maintain credit card account information including balances, credit limits, and customer details. The system must support both online inquiry/update operations for customer service staff and automated batch processing for account maintenance tasks such as interest calculation and cycle resets.

## Functional Requirements

### FR-002.1: Account Inquiry
**Description**: System must enable users to view complete account information including balances, credit limits, status, and associated customer details.

**User Need**: Customer service representatives need quick access to account information to answer customer inquiries and resolve issues.

**Source**: PROG-COACTVWC.md - Account viewing functionality with three-tier data retrieval (CARDXREF → ACCTDAT → CUSTDAT)

**Business Rules**:
- Account retrieval requires valid 11-digit account number
- System must display current balance, credit limit, and cash credit limit
- System must show associated customer information (name, contact details)
- System must display associated card number
- Account status must be clearly indicated

### FR-002.2: Account and Customer Update
**Description**: System must allow authorized users to update account information (status, limits, balances, dates) and associated customer personal information (name, address, contact details) in a single integrated transaction.

**User Need**: Customer service staff need to maintain accurate account and customer information while ensuring data consistency across related records.

**Source**: PROG-COACTUPC.md - Comprehensive update program with 40+ editable fields and two-file transactional update

**Business Rules**:
- Updates must maintain transactional consistency (both account and customer update together or neither)
- All field modifications must be validated before commit
- System must detect and prevent concurrent modifications
- Credit limit must be greater than or equal to current balance
- Account expiration date must be greater than or equal to open date
- System must support updating: account status, balances, credit limits, dates, group ID, customer name, address, contact information, SSN, date of birth, FICO score

### FR-002.3: Interest Calculation
**Description**: System must automatically calculate and apply monthly interest charges to account balances based on transaction category balances and configured interest rates.

**User Need**: Business requires automated interest accrual to generate revenue and maintain accurate account balances according to cardholder agreements.

**Source**: PROG-CBACT04C.md - Monthly batch interest calculation with rate lookup and transaction generation

**Business Rules**:
- Interest calculated monthly on transaction category balances
- Interest rate varies by account group and transaction category
- Formula: Monthly Interest = (Category Balance × Annual Rate) / 1200
- System must support default rates when specific rate not found
- Generated interest transactions must be recorded in transaction history
- Account balances must be updated with total interest amount

### FR-002.4: Billing Cycle Management
**Description**: System must support monthly billing cycle processing including resetting cycle-to-date counters and maintaining cycle history.

**User Need**: Business requires accurate billing cycle tracking for statement generation and credit management.

**Source**: PROG-CBACT04C.md - Cycle reset logic that zeros cycle credit/debit counters

**Business Rules**:
- Billing cycle runs monthly
- Cycle-to-date credit and debit counters must reset to zero at cycle end
- Current balance must be maintained across cycles
- System must preserve historical cycle data for reporting

### FR-002.5: Account Data Export
**Description**: System must support exporting account data in multiple formats for reporting and integration with downstream systems.

**User Need**: Business requires account data in various formats for reporting, data warehouse integration, and audit purposes.

**Source**: PROG-CBACT01C.md - Account file export utility with multiple output formats

**Business Rules**:
- Support sequential file format for standard exports
- Support array-based format for structured data exchange
- Support variable-length record format for flexible processing
- Exports must include all account fields with proper date formatting

## Business Rules

### Rule 002-1: Account Number Format
**Statement**: Account numbers must be exactly 11 numeric digits with no special characters.

**Rationale**: Ensures consistent identification and prevents input errors.

**Source**: PROG-COACTVWC.md - Account ID validation logic

### Rule 002-2: Credit Limit Constraint
**Statement**: Account credit limit must always be greater than or equal to current balance.

**Rationale**: Prevents system from accepting payments that would result in credit limit violation.

**Source**: PROG-COACTUPC.md - Credit limit validation

### Rule 002-3: Date Logical Relationships
**Statement**: Account opened date must be less than or equal to expiration date; reissue date (if present) must be greater than or equal to opened date.

**Rationale**: Ensures logical consistency of account lifecycle dates.

**Source**: PROG-COACTUPC.md - Date validation with CSUTLDWY utility

### Rule 002-4: Transactional Integrity
**Statement**: Account and customer updates must be atomic - both succeed or both fail.

**Rationale**: Prevents data inconsistency between related master files.

**Source**: PROG-COACTUPC.md - Two-phase commit pattern with SYNCPOINT ROLLBACK

### Rule 002-5: Optimistic Concurrency
**Statement**: System must detect concurrent modifications and reject updates if record has been changed since initial read.

**Rationale**: Prevents lost updates in multi-user environment.

**Source**: PROG-COACTUPC.md - Change detection logic with 30+ field comparisons

### Rule 002-6: Interest Rate Hierarchy
**Statement**: Interest rates are determined by account group ID, transaction type, and category code; if specific rate not found, use DEFAULT group rate.

**Rationale**: Provides flexible rate configuration while ensuring all transactions have applicable rate.

**Source**: PROG-CBACT04C.md - Interest rate lookup with fallback to DEFAULT

### Rule 002-7: FICO Score Range
**Statement**: FICO credit scores must be between 300 and 850 inclusive.

**Rationale**: Aligns with standard FICO scoring methodology.

**Source**: PROG-COACTUPC.md - FICO score validation (1275-EDIT-FICO-SCORE)

## Data Requirements

### Entity: Account
**Purpose**: Represents a credit card account with associated balances, limits, and lifecycle information.

**Key Attributes**:
- Account ID (11-digit unique identifier)
- Account status (active/inactive)
- Current balance (monetary amount)
- Credit limit (maximum credit allowed)
- Cash credit limit (maximum cash advance)
- Current cycle credit total (cycle-to-date credits)
- Current cycle debit total (cycle-to-date debits)
- Opened date, expiration date, reissue date
- Account group ID (for rate determination)

**Relationships**:
- One account belongs to one customer
- One account may have multiple cards
- One account has many transactions
- One account has many transaction category balances

### Entity: Customer
**Purpose**: Represents an individual customer with personal and contact information.

**Key Attributes**:
- Customer ID (unique identifier)
- Name (first, middle, last)
- Social Security Number
- Date of birth
- FICO credit score
- Address (street, city, state, ZIP, country)
- Phone numbers (home, work, mobile)
- Government ID
- EFT routing account
- Primary card holder flag

**Relationships**:
- One customer may have multiple accounts
- Customer information shared across all accounts

### Entity: Transaction Category Balance
**Purpose**: Tracks accumulated balances by transaction category for interest calculation.

**Key Attributes**:
- Account ID
- Transaction type code
- Transaction category code
- Balance amount

**Relationships**:
- Many category balances per account
- Used for interest calculation

## User Personas Affected

### Customer Service Representative
**Interaction**: Views account details to answer customer questions, updates account information when customer requests changes (address, phone, credit limit increase), processes account maintenance requests.

**Needs**: Fast account lookup, clear display of all relevant information, ability to update multiple related fields in one operation, confidence that data is current and consistent.

### Account Manager
**Interaction**: Reviews account status, analyzes balances and credit utilization, approves credit limit changes, monitors account health.

**Needs**: Comprehensive account view, ability to see customer history, tools to assess creditworthiness, access to FICO scores and payment behavior.

### Billing Processor (Automated)
**Interaction**: System automatically calculates interest, resets billing cycles, generates interest transactions, updates balances.

**Needs**: Reliable automated processing, accurate interest calculations, proper error handling, audit trail of batch operations.

### Data Analyst
**Interaction**: Exports account data for analysis, creates reports on account portfolio, analyzes trends and patterns.

**Needs**: Access to complete account datasets, multiple export formats, consistent data structure, historical data preservation.

## Non-Functional Requirements

### Performance
- Account inquiry must respond within 2 seconds
- Account updates must complete within 3 seconds
- Interest calculation batch must process 100,000 accounts within 2 hours
- Data export must handle 500,000 account records efficiently

### Security
- Account access requires authentication and authorization
- Sensitive customer data (SSN, FICO) must be protected
- All account modifications must be audited
- Access to financial data must be role-based

### Usability
- Account lookup must support search by account number, card number, or customer name
- Validation errors must clearly indicate which field has issue and why
- System must prevent data loss through confirmation prompts on updates
- Related information (customer, cards) must be easily accessible from account view

### Compliance
- System must maintain audit trail of all account changes
- Interest calculation must comply with Truth in Lending Act
- Customer data handling must comply with privacy regulations
- System must support financial reporting requirements

### Reliability
- Account updates must be atomic (all-or-nothing)
- System must handle concurrent access without data corruption
- Interest calculation must be accurate to the penny
- System must recover gracefully from failures without data loss

## Success Criteria

- [ ] Users can retrieve account information in under 2 seconds with 99.9% success rate
- [ ] Account updates maintain 100% transactional integrity (no partial updates)
- [ ] Concurrent update detection prevents 100% of lost update scenarios
- [ ] Interest calculation produces accurate results matching manual calculations
- [ ] Billing cycle processing completes successfully for all accounts
- [ ] Data exports produce valid output in all supported formats
- [ ] Zero data corruption incidents due to concurrent access
- [ ] All account modifications are successfully audited
- [ ] Customer service staff report improved efficiency in account maintenance
- [ ] Batch processing completes within defined time windows
