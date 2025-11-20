# UC-009: Search and Browse Cards

## Overview
**Actor**: Customer Service Representative, Card Operations Specialist  
**Goal**: Search for and browse credit cards using various filter criteria with paginated navigation  
**Frequency**: High - Multiple times per hour during business operations  
**Priority**: High - Primary entry point for card management  

## Preconditions
- User is authenticated and authorized to access card information
- User has access to card management functionality
- Card master file contains card records

## Main Success Scenario
1. User navigates to card search page
2. System displays empty search form with optional filters:
   - Account number field
   - Card number field
3. User enters filter criteria (account number, card number, or both)
4. User submits search
5. System validates filter inputs
6. System queries card master file based on filter criteria
7. System displays first page of results (7-20 cards depending on configuration) showing:
   - Card number (partially masked if security required)
   - Account ID
   - Card status (Active/Inactive)
   - Expiration date
   - Embossed name
   - Selection column with action options (View/Update)
8. System displays pagination controls if more results available
9. User reviews results
10. User can:
    - Navigate to next/previous page
    - Select a card for viewing (enter 'S' or click View)
    - Select a card for updating (enter 'U' or click Edit)
    - Modify filter criteria and search again
    - Exit to main menu

## Alternative Flows

### If user enters account number only
- System queries cards by account using alternate index
- System displays all cards associated with that account
- Results may show multiple cards per account

### If user enters card number only
- System performs direct lookup using card number as primary key
- System typically returns single matching card
- System displays in list format for consistency

### If user enters both account and card number
- System uses card number (more specific) for primary lookup
- System validates card belongs to specified account
- System displays matching card if account-card relationship valid

### If user enters no filter criteria (blank search)
- System displays all cards (with pagination)
- System may limit results to first 100 or 1000 cards for performance
- System suggests adding filter criteria for better performance

### If user navigates to next page (F8 or Next button)
- System retrieves next set of results from current position
- System displays subsequent cards
- System disables/hides "Next" button if at last page

### If user navigates to previous page (F7 or Previous button)
- System repositions browse cursor backward
- System displays previous set of cards
- System disables/hides "Previous" button if at first page

## Exception Flows

### If no cards match filter criteria
- System displays message: "No cards found matching search criteria"
- System preserves filter values in form
- User can modify criteria and search again

### If filter validation fails (invalid format)
- System displays validation error
- System positions cursor in error field
- User corrects input and resubmits

### If card file access error occurs
- System displays error message: "Unable to retrieve card data"
- System logs technical error for support
- User can retry or contact support

### If pagination state is lost (session timeout)
- System returns to initial empty search form
- System displays session timeout message
- User must re-enter search criteria

## Business Rules Applied
- BR-003: Card Management
  - Rule 003-4: Single selection (only one card can be selected at a time)
- Filter criteria preserved across page navigation
- Results displayed in sorted order (by card number or account number)

## UI/UX Considerations
- Clearly indicate which filter fields are used for current results
- Show total result count: "Showing 1-20 of 142 cards"
- Highlight expired cards in results (different color or icon)
- Show card status visually (green for active, red for inactive)
- Mask card numbers appropriately (show last 4 digits only if required)
- Provide sort capabilities (click column header to sort)
- Support keyboard shortcuts for power users (Enter to search, F7/F8 for pagination)
- Auto-focus on account number field when page loads
- Clear button to reset all filters quickly

## Acceptance Criteria
- [ ] User can search by account number only
- [ ] User can search by card number only
- [ ] User can search by both account and card number
- [ ] User can browse all cards with no filters
- [ ] System displays results within 2 seconds
- [ ] Pagination controls work correctly (forward and backward)
- [ ] Filter criteria preserved when navigating between pages
- [ ] User can select one card for viewing with 'S' or View button
- [ ] User can select one card for updating with 'U' or Edit button
- [ ] System validates only one selection per page
- [ ] System displays appropriate message when no results found
- [ ] System handles large result sets (10,000+ cards) efficiently
- [ ] Expired cards are visually distinguished in results

## Source
**COBOL Program**: COCRDLIC  
**Business Requirement**: BR-003 (FR-003.1: Card Search and List)
