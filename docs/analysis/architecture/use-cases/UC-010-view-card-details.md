# UC-010: View Card Details

## Overview
**Actor**: Customer Service Representative, Card Operations Specialist, Fraud Prevention Analyst  
**Goal**: View complete card information in read-only mode without ability to modify  
**Frequency**: High - Frequent card detail lookups during customer interactions  
**Priority**: High - Essential for card verification and inquiry  

## Preconditions
- User is authenticated and authorized to view card information
- User has selected a card from card list or has card number
- Card exists in the system

## Main Success Scenario
1. User arrives at card detail view either by:
   - Selecting 'S' (View) from card search results
   - Entering card number and account ID directly
2. System retrieves card record from card master file
3. System retrieves associated account information
4. System retrieves associated customer information via cross-reference
5. System displays read-only card details including:
   - Card number (full or partially masked based on permissions)
   - Embossed name (name on card)
   - Active status (Active/Inactive)
   - Expiration date (month/year)
   - Associated account ID
   - Customer name
   - CVV code (if user has permission, otherwise masked)
6. User reviews card information
7. User can:
   - Press F3 or Back button to return to card list
   - Enter different account/card number to view another card
   - Navigate to Edit if user has update permissions

## Alternative Flows

### If user enters different card number to view
- System validates new card number format
- System retrieves new card record
- System displays new card details
- System maintains navigation context

### If user navigates to card update from detail view
- System transfers to card update use case (UC-011)
- System passes card identification information
- User can make modifications in update mode

### If user accesses expired card
- System displays card details normally
- System highlights expiration date in red or with warning
- System may display "EXPIRED" indicator prominently

### If user accesses inactive card
- System displays card details normally
- System shows status as "Inactive" with clear visual indicator
- System may display reason for inactivation if available

## Exception Flows

### If card not found
- System displays error: "Card not found in system"
- System returns to search page or clears card number field
- User can search for different card

### If account-to-card relationship validation fails
- System displays error: "Card does not belong to specified account"
- System may show card details with warning
- User can verify account and card numbers

### If associated customer record not found
- System displays card details with partial information
- System shows error: "Associated customer information not available"
- System logs data integrity issue for investigation
- User can still see card-specific fields

### If user lacks permission to view CVV
- System masks CVV field (shows *** or blank)
- System displays all other card information normally
- User sees appropriate message about restricted access

### If file access error occurs
- System displays error: "Unable to retrieve card information"
- System logs technical error with details
- User can retry or contact support

## Business Rules Applied
- BR-003: Card Management
  - FR-003.2: Read-only card detail view
- Card access must be audited for security compliance
- CVV display restricted based on user role/permissions

## UI/UX Considerations
- Clear visual indication this is read-only view (no editable fields)
- Display card information in logical groups:
  - Card identification (number, embossed name)
  - Status (active/inactive, expiration)
  - Associated account/customer
  - Security (CVV if permitted)
- Show expiration status prominently (expired, expiring soon, valid)
- Provide visual card representation (card-shaped element with details)
- Include action buttons: Back to List, Edit (if permitted)
- Display last access or last update timestamp
- Show related information links (view account, view transactions)

## Acceptance Criteria
- [ ] User can view card details from search results selection
- [ ] User can view card details by entering account and card number
- [ ] System displays all card information fields
- [ ] System loads card details within 1 second
- [ ] All fields are read-only (no editing possible)
- [ ] Expired cards are visually highlighted
- [ ] Inactive cards show status clearly
- [ ] CVV is masked for users without appropriate permissions
- [ ] System validates account-to-card relationship
- [ ] System displays associated customer information
- [ ] User can return to card list with preserved search context
- [ ] User can view different card from detail page
- [ ] System audits card detail access for compliance

## Source
**COBOL Program**: COCRDSLC  
**Business Requirement**: BR-003 (FR-003.2: Card Detail View)
