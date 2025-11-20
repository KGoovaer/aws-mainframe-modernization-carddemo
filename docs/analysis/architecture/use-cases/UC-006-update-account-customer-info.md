# UC-006: Update Account and Customer Information

## Overview
**Actor**: Customer Service Representative, Account Manager  
**Goal**: Update account settings and associated customer personal information in a single transaction  
**Frequency**: Medium - Several times per day for account maintenance  
**Priority**: High - Critical for maintaining accurate customer data  

## Preconditions
- User is authenticated and authorized to update accounts
- User has accessed account through inquiry or search
- Account and customer records exist in system
- No other user is currently updating this account

## Main Success Scenario
1. User navigates to account update page with account number
2. System retrieves and locks account record
3. System retrieves associated customer record
4. System displays current values in editable form with 40+ fields including:
   - Account: status, balances, credit limits, dates, group ID
   - Customer: name, address, phone, SSN, DOB, FICO score
5. User modifies one or more fields
6. User submits changes
7. System validates all modified fields:
   - Account status (Y/N only)
   - Numeric fields (balances, limits, dates)
   - Credit limit >= current balance
   - Date logical relationships (opened <= expiration)
   - Customer name (alphabetic only)
   - SSN format and validity
   - Phone number format (US)
   - State code validity
   - ZIP code format and state matching
   - FICO score range (300-850)
8. System displays validation results
9. If all validations pass:
   - System reveals Save and Cancel options
   - System displays "Press F5 to Save, F12 to Cancel"
10. User presses F5 to confirm
11. System performs concurrency check (detects if record changed by another user)
12. System updates both account and customer records atomically
13. System releases locks
14. System displays success message
15. System returns to account list or inquiry page

## Alternative Flows

### If validation fails on any field
- System displays error message indicating which field failed and why
- System highlights error field in red
- System positions cursor at first error field
- System preserves user's other inputs
- User corrects errors and resubmits

### If user presses F12 to cancel
- System releases record locks
- System discards all changes
- System returns to previous page
- User sees "Update cancelled" message

### If user exits (F3) before saving
- System releases record locks
- System discards all changes
- System returns to account list

### If multiple fields have errors
- System displays first error encountered
- After user corrects and resubmits, system displays next error
- Process repeats until all validations pass

## Exception Flows

### If concurrent modification detected
- System displays message: "Data was changed by another user"
- System re-reads current record values from database
- System displays updated values
- User can review changes made by other user
- User can re-apply their changes or cancel
- System does NOT automatically commit user's changes

### If record lock acquisition fails
- System displays error: "Unable to lock record for update"
- System suggests user try again later
- System returns to inquiry page

### If account update succeeds but customer update fails
- System performs automatic rollback
- System displays error: "Update failed - no changes were saved"
- System releases locks
- User can retry entire operation

### If user loses connection during update
- System automatically releases locks after timeout
- Changes are not saved
- User must restart update process

## Business Rules Applied
- BR-002: Account Management
  - Rule 002-2: Credit limit constraint
  - Rule 002-3: Date logical relationships
  - Rule 002-4: Transactional integrity
  - Rule 002-5: Optimistic concurrency
  - Rule 002-7: FICO score range
- All field-level validation rules from COACTUPC validation framework

## UI/UX Considerations
- Display current values in all fields (not blank form)
- Mark required fields clearly
- Show asterisk (*) for required fields that are blank
- Use different colors for valid fields (normal) and error fields (red)
- Provide clear, specific error messages (not generic "invalid input")
- Show field format hints (e.g., "SSN: XXX-XX-XXXX", "Phone: (XXX)XXX-XXXX")
- Group related fields (Account section, Personal Info section, Address section)
- Hide Save/Cancel buttons until validation passes
- Require explicit confirmation to prevent accidental updates
- Show which fields were actually modified in confirmation summary

## Acceptance Criteria
- [ ] User can update account status, limits, balances, and dates
- [ ] User can update customer name, address, contact information
- [ ] System validates all fields according to business rules
- [ ] System prevents updates when validation fails
- [ ] System requires explicit confirmation before committing changes
- [ ] System maintains transactional integrity (both files update or neither)
- [ ] System detects and prevents concurrent modification conflicts
- [ ] System provides clear error messages for validation failures
- [ ] System positions cursor at first error field
- [ ] System completes update within 3 seconds
- [ ] System audits all successful updates with user ID and timestamp

## Source
**COBOL Program**: COACTUPC (4,237 lines - most complex program)  
**Business Requirement**: BR-002 (FR-002.2: Account and Customer Update)
