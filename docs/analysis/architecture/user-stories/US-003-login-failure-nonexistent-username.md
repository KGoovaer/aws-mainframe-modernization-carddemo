# US-003: Login Failure with Non-Existent Username

## User Story
**As a** user attempting to log in  
**I want to** receive a clear error message when I enter a username that doesn't exist  
**So that** I know to check my username or contact support if needed

## Source
**COBOL Program**: COSGN00C (user validation, RESP 13 handling)  
**Business Requirement**: BR-001 (User Authentication - FR-001.4)  
**Use Case**: UC-003 (Authentication Failure Recovery - Alternative Flow 3a)

## Acceptance Criteria

**Given** I enter a username that does not exist in the system  
**When** I submit the login form  
**Then** I see an error message stating "User identifier not recognized. Please verify and try again."

**Given** authentication fails due to non-existent username  
**When** the error is displayed  
**Then** both the username and password fields are cleared for security

**Given** authentication fails due to non-existent username  
**When** the error is displayed  
**Then** the cursor is positioned in the username field

**Given** failed authentication due to non-existent username  
**When** the error occurs  
**Then** the failed attempt is logged with entered username, timestamp, and error type

**Given** I mistyped my username  
**When** I see the error message  
**Then** I can immediately re-enter my correct username and try again

## Business Rules
- Error message doesn't reveal whether user exists for security (similar wording to password error)
- Both fields cleared to prevent security exposure
- Attempt still counts toward account lockout threshold (prevents username enumeration)
- All failed attempts logged regardless of reason

## UI/UX Considerations
- Error message displayed clearly in red
- Both username and password fields cleared
- Cursor positioned in username field for re-entry
- Error message is specific enough to be helpful
- Error wording does not confirm user existence (security)
- Accessible error announcement

## Security Considerations
- Error message carefully worded to not confirm user existence
- Both fields cleared to prevent information exposure
- Logging includes attempted username for security monitoring
- Rate limiting prevents username enumeration attacks
- Similar treatment for non-existent users and wrong passwords

## Technical Notes
- User lookup performed against user security database
- Failed attempt counter may still increment (policy-dependent)
- Security monitoring may flag repeated non-existent user attempts
- Distinguish legitimate typos from enumeration attacks in logs

## Definition of Done
- [x] Non-existent username displays appropriate error message
- [x] Both username and password fields are cleared
- [x] Cursor positioned in username field
- [x] Failed attempt is logged with details
- [x] Error message is accessible
- [x] Security considerations addressed (no username confirmation)
- [x] User can immediately retry
- [x] Rate limiting prevents enumeration
