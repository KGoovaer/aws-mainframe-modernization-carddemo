# UC-005: View Account Details

## Overview
**Actor**: Customer Service Representative, Account Manager  
**Goal**: View comprehensive account information including balances, limits, customer details, and associated card information  
**Frequency**: High - Multiple times per hour during business operations  
**Priority**: High - Core inquiry functionality  

## Preconditions
- User is authenticated and authorized to view account information
- User has access to account management functionality
- Account exists in the system

## Main Success Scenario
1. User navigates to account inquiry page
2. User enters 11-digit account number in search field
3. System validates account number format (numeric, 11 digits, non-zero)
4. System retrieves account information from account master file
5. System retrieves associated customer information
6. System retrieves associated card information
7. System displays comprehensive account view including:
   - Account ID and status
   - Current balance, credit limit, cash credit limit
   - Cycle-to-date credits and debits
   - Account dates (opened, expiration, reissue)
   - Customer name and contact information
   - Associated card number
8. User reviews account information
9. User can enter different account number to view another account or exit

## Alternative Flows

### If account number format is invalid
- System displays validation error: "Account number must be a non-zero 11-digit number"
- System positions cursor in account number field
- User corrects input and resubmits

### If account not found in cross-reference file
- System displays error: "Account not found in system"
- System clears account number field
- User can enter different account number or exit

### If associated customer not found
- System displays error: "Associated customer information not found"
- System shows partial account information (account data only)
- User can report data integrity issue

### If user searches by card number instead
- System performs alternate search using card-to-account cross-reference
- Retrieves associated account number
- Continues with main success scenario from step 4

## Exception Flows

### If file access error occurs
- System displays technical error message with response codes
- System logs error for technical support
- User can retry or contact support

### If user lacks authorization
- System displays "Access denied" message
- System logs unauthorized access attempt
- User is returned to previous page

## Business Rules Applied
- BR-002: Account Management (Rule 002-1: Account number format)
- Account retrieval requires three-step data access: cross-reference → account → customer

## Acceptance Criteria
- [ ] User can retrieve account by entering valid account number
- [ ] System displays complete account information in under 2 seconds
- [ ] System validates account number format before database access
- [ ] System displays appropriate error messages for invalid inputs
- [ ] System shows associated customer and card information
- [ ] User can perform multiple sequential inquiries without logging out
- [ ] System prevents unauthorized access to account information

## Source
**COBOL Program**: COACTVWC  
**Business Requirement**: BR-002 (FR-002.1: Account Inquiry)
