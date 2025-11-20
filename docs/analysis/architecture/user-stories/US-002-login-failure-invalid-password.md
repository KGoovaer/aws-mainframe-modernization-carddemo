# US-002: Login Failure with Invalid Password

## User Story
**As a** customer service representative  
**I want to** receive a clear error message when I enter the wrong password  
**So that** I understand the problem and can retry with the correct password

## Source
**COBOL Program**: COSGN00C (error handling, lines 350-400)  
**Business Requirement**: BR-001 (User Authentication - FR-001.4)  
**Use Case**: UC-003 (Authentication Failure Recovery)

## Acceptance Criteria

**Given** I have a valid username but enter an incorrect password  
**When** I submit the login form  
**Then** I see an error message stating "Password incorrect. Please try again."

**Given** authentication fails due to wrong password  
**When** the error is displayed  
**Then** the password field is cleared but my username is retained in the form

**Given** authentication fails  
**When** the error is displayed  
**Then** the cursor is positioned in the password field for easy retry

**Given** this is my first failed attempt  
**When** the error is displayed  
**Then** I see an indication of attempt count: "Attempt 1 of 5"

**Given** failed authentication due to wrong password  
**When** the error occurs  
**Then** the failed attempt is logged with my username, timestamp, and error type

## Business Rules
- Password field must be cleared on authentication failure (security)
- Username field retained for user convenience
- Failed attempt counter increments on each failure
- Maximum 5 failed attempts before account lockout

## UI/UX Considerations
- Error message displayed in red with appropriate icon
- Error message is clear and actionable
- Field focus automatically set to password field
- Username field pre-populated with previously entered value
- Password field empty for security
- Error message accessible to screen readers

## Technical Notes
- Failed attempt counter stored server-side
- Cannot be manipulated by client
- Approach account lockout threshold tracked per user
- Security event logged for monitoring

## Definition of Done
- [x] Wrong password displays appropriate error message
- [x] Password field is cleared on error
- [x] Username field is retained
- [x] Cursor positioned in password field
- [x] Attempt counter displayed and accurate
- [x] Failed attempt is logged
- [x] Error message is accessible
- [x] User can immediately retry
