# UC-007: Calculate Monthly Interest

## Overview
**Actor**: System (Automated Batch Process)  
**Goal**: Calculate and apply monthly interest charges to all credit card accounts based on category balances and interest rates  
**Frequency**: Monthly - Scheduled batch job at end of billing cycle  
**Priority**: Critical - Revenue generation and account balance accuracy  

## Preconditions
- Transaction posting has completed for the billing cycle
- Transaction category balances are up-to-date
- Interest rate tables are properly configured
- Account master file is accessible for updates

## Main Success Scenario
1. System initiates monthly interest calculation batch job
2. System reads transaction category balance file sequentially (sorted by account)
3. For each account:
   - System accumulates total interest across all categories
   - For each transaction category with non-zero balance:
     - System looks up interest rate by account group, transaction type, category
     - System calculates monthly interest: (Balance × Annual Rate) / 1200
     - System adds to account's total interest accumulator
4. When account number changes (control break):
   - System reads account master record using READ UPDATE (locks record)
   - System reads card cross-reference to get card number
   - System generates interest transaction record:
     - Transaction ID: Processing date + sequence suffix
     - Type: Debit/Charge
     - Category: Interest
     - Amount: Total calculated interest for account
     - Description: "Interest for account {account-id}"
     - Source: System
     - Timestamps: Current date/time
   - System writes interest transaction to transaction file
   - System adds total interest to account current balance
   - System resets cycle-to-date credit and debit counters to zero
   - System updates account record (REWRITE)
   - System releases lock
5. System continues until all accounts processed
6. System generates summary report:
   - Total accounts processed
   - Total interest generated
   - Any errors or accounts skipped
7. System completes successfully

## Alternative Flows

### If interest rate not found for specific group/type/category
- System attempts lookup using DEFAULT group code
- If DEFAULT rate found: uses that rate for calculation
- If DEFAULT rate also not found: logs error, skips category, continues processing
- System generates exception report for missing rates

### If account has zero balances in all categories
- System skips interest calculation for account
- System still resets cycle counters
- System continues to next account

### If interest calculation results in zero or negative
- System skips transaction generation (no interest transaction created)
- System resets cycle counters
- System continues to next account

## Exception Flows

### If account record lock acquisition fails
- System logs error with account number and reason
- System skips interest for this account
- System adds account to exception report
- System continues processing remaining accounts
- Unprocessed accounts must be handled in next run or manual correction

### If account master file read fails
- System logs error with account number and file response codes
- System attempts to continue with next account
- System generates alert for technical support
- Critical file errors may abort entire job

### If transaction file write fails
- System logs error with account and transaction details
- System does not update account balance (maintains consistency)
- System adds to exception report
- System continues processing remaining accounts

### If job abends (abnormal termination)
- System ensures no partial account updates (atomicity)
- Restart procedures should reprocess from beginning or checkpoint
- Manual review required to verify no duplicate interest charges

## Business Rules Applied
- BR-002: Account Management
  - Rule 002-6: Interest rate hierarchy (specific rate, then DEFAULT)
- Interest calculation formula: Monthly Interest = (Category Balance × Annual Rate) / 1200
- Cycle counter reset indicates end of billing cycle
- Interest transactions are system-generated (not user-entered)

## Acceptance Criteria
- [ ] System processes all accounts with transaction category balances
- [ ] System calculates interest accurately using correct rates
- [ ] System applies default rate when specific rate not configured
- [ ] System generates one interest transaction per account (if applicable)
- [ ] System updates account balances with total interest amount
- [ ] System resets cycle-to-date credit and debit counters to zero
- [ ] System maintains transactional integrity (balance update matches transaction)
- [ ] System handles missing rates gracefully without aborting entire job
- [ ] System completes processing within 2-hour time window for 100K accounts
- [ ] System generates comprehensive summary and exception reports
- [ ] Interest calculations match manual verification within 1 cent
- [ ] System creates audit trail of all interest charges

## Source
**COBOL Program**: CBACT04C  
**Business Requirement**: BR-002 (FR-002.3: Interest Calculation, FR-002.4: Billing Cycle Management)
