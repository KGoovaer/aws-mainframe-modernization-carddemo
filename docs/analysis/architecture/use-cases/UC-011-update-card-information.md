# UC-011: Update Card Information

## Overview
**Actor**: Customer Service Representative, Card Operations Specialist  
**Goal**: Update card embossed name, active status, or expiration date with validation and concurrency control  
**Frequency**: Medium - Several times per day for card maintenance  
**Priority**: High - Critical for maintaining accurate card data  

## Preconditions
- User is authenticated and authorized to update cards
- User has selected card from list or has card number
- Card exists in the system
- No other user is currently updating this card

## Main Success Scenario
1. User arrives at card update page by:
   - Selecting 'U' (Update) from card search results
   - Clicking Edit button from card detail view
2. System retrieves and locks card record using READ UPDATE
3. System retrieves associated account and customer information for context
4. System displays editable form with current values:
   - Card embossed name (editable text field)
   - Active status (Y/N dropdown or toggle)
   - Expiration month (dropdown 1-12)
   - Expiration year (numeric input 1950-2099)
   - Account ID (read-only, for context)
   - Customer name (read-only, for context)
5. User modifies one or more fields
6. User submits changes (presses Enter or Submit button)
7. System validates modified fields:
   - Card name: alphabetic and spaces only, not blank, max 50 characters
   - Active status: exactly 'Y' or 'N'
   - Expiration month: numeric, 1-12
   - Expiration year: numeric, 1950-2099, not expired
8. If all validations pass:
   - System displays validation success message
   - System reveals Save (F5) and Cancel (F12) buttons
   - System shows "Press F5 to Save or F12 to Cancel"
9. User presses F5 to confirm save
10. System performs concurrency check (verifies record not changed by another user)
11. System updates card record (REWRITE)
12. System releases lock
13. System displays success message: "Card updated successfully"
14. System returns to card list or detail view

## Alternative Flows

### If user presses F12 to cancel before saving
- System releases record lock
- System discards all changes
- System displays "Update cancelled"
- System returns to previous page (list or detail)

### If user presses F3 to exit before confirming
- System releases record lock
- System discards all changes
- System returns to card list

### If user modifies expiration date to future date
- System validates month (1-12) and year (1950-2099)
- System calculates combined expiration date
- System ensures date is in future
- System accepts if valid

### If user changes status from Active (Y) to Inactive (N)
- System processes as normal update
- System may display confirmation: "You are deactivating this card"
- User confirms action
- Card status updated to inactive

### If user changes status from Inactive (N) to Active (Y)
- System processes as normal update
- System may validate card not expired before reactivation
- System warns if expiration date is soon or past

## Exception Flows

### If validation fails on card name (contains numbers or special characters)
- System displays error: "Card name must contain only letters and spaces"
- System highlights card name field in red
- System positions cursor in card name field
- System preserves other user inputs
- User corrects name and resubmits

### If validation fails on active status (not Y or N)
- System displays error: "Status must be Y or N"
- System highlights status field
- User selects valid status value and resubmits

### If validation fails on expiration date (month out of range)
- System displays error: "Month must be between 1 and 12"
- System highlights expiration month field
- User corrects month and resubmits

### If validation fails on expiration date (year out of range)
- System displays error: "Year must be between 1950 and 2099"
- System highlights expiration year field
- User corrects year and resubmits

### If validation fails on expiration date (date in past)
- System displays error: "Expiration date cannot be in the past"
- System highlights both month and year fields
- User updates to future date and resubmits

### If concurrent modification detected after confirmation
- System displays error: "Card was updated by another user"
- System re-reads current card values from database
- System displays updated values
- System releases lock
- User sees latest card data
- User can review changes made by other user
- User can re-apply their changes or cancel

### If record lock acquisition fails
- System displays error: "Card is currently being updated by another user"
- System suggests trying again in a moment
- System returns to card detail or list view

### If database update (REWRITE) fails
- System displays error: "Unable to update card - please try again"
- System releases lock
- System logs technical error
- User can retry operation

### If user session times out during update
- System automatically releases record lock
- Changes are not saved
- User sees session timeout message
- User must log in and restart update process

## Business Rules Applied
- BR-003: Card Management
  - Rule 003-1: Card name format (alphabetic and spaces only)
  - Rule 003-2: Active status values (Y or N only)
  - Rule 003-3: Expiration date validity
  - Rule 003-5: Update confirmation required
  - Rule 003-6: Optimistic concurrency control

## UI/UX Considerations
- Display current values in all fields (not blank form)
- Clearly indicate which fields are editable vs. read-only
- Provide field format hints (e.g., "Name: Letters and spaces only")
- Show field character counts for name field (e.g., "25/50")
- Use dropdown for active status to prevent invalid entry
- Use date picker or separate month/year dropdowns for expiration
- Hide Save/Cancel buttons until validation passes
- Show validation errors inline near affected field
- Provide "Review Changes" summary before final commit
- Show what changed: "Embossed Name: 'John Smith' → 'John Q Smith'"
- Highlight modified fields with visual indicator
- Provide clear confirmation message after successful update

## Acceptance Criteria
- [ ] User can update card embossed name with valid characters
- [ ] User can change card active status between Y and N
- [ ] User can update expiration month (1-12) and year (1950-2099)
- [ ] System validates card name accepts only letters and spaces
- [ ] System validates expiration date is not in the past
- [ ] System requires explicit confirmation (F5) before committing changes
- [ ] System allows cancellation (F12) before commit
- [ ] System detects concurrent modifications and prevents lost updates
- [ ] System completes update within 2 seconds
- [ ] System displays clear error messages for validation failures
- [ ] System positions cursor at first error field
- [ ] System audits all card updates with user ID and timestamp
- [ ] System releases locks on cancel or exit
- [ ] User cannot update card being edited by another user

## Source
**COBOL Program**: COCRDUPC  
**Business Requirement**: BR-003 (FR-003.3: Card Information Update)
