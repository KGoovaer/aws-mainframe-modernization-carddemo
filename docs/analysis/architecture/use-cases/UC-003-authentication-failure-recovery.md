# UC-003: Authentication Failure Recovery

## Overview
**Actor**: Customer Service Representative, Administrative Staff (any user attempting to login)  
**Goal**: Successfully authenticate after initial login failure  
**Frequency**: Occasional - when user makes typo, forgets password, or has account issue  
**Priority**: High - Critical for user access and reducing support burden

## Preconditions
- User has been provisioned with valid user account
- User has attempted to login and received authentication error
- System is available and operational

## Main Success Scenario
1. User attempts to login with credentials
2. System validates credentials and detects error (wrong password, user not found, etc.)
3. System displays specific, actionable error message
4. System maintains user identifier in form (if user not found error, may clear it)
5. System clears password field
6. System positions cursor appropriately based on error type
7. User reads error message and understands issue
8. User corrects the error (re-enters password, corrects user identifier, etc.)
9. User submits login form again
10. System successfully authenticates user
11. User gains access to application

## Alternative Flows

### 3a: User Identifier Not Found
**If** user identifier does not exist
- System displays: "User identifier not recognized. Please verify and try again."
- System clears password field for security
- System positions cursor on user identifier field
- User corrects user identifier
- Continue with step 9

### 3b: Password Incorrect (First Attempt)
**If** password is incorrect but account not yet locked
- System displays: "Password incorrect. Please try again."
- System displays failed attempt count: "Attempt 1 of 5"
- System clears password field
- System maintains user identifier
- System positions cursor on password field
- User re-enters password
- Continue with step 9

### 3c: Password Incorrect (Multiple Attempts)
**If** password is incorrect and this is 2nd, 3rd, or 4th attempt
- System displays: "Password incorrect. Please try again."
- System displays warning: "Attempt X of 5. Account will be locked after 5 failed attempts."
- System clears password field
- System provides "Forgot Password?" link prominently
- User can retry or click "Forgot Password" link
- Continue with step 9 or go to password reset process

### 8a: User Chooses Password Reset
**If** user clicks "Forgot Password?" or "Reset Password" link
- System transitions to password reset workflow (UC-006)
- User completes password reset process
- User returns to login with new password
- Continue with step 1

### 8b: User Contacts Support
**If** user cannot resolve issue independently
- User contacts help desk or system administrator
- Administrator investigates account status
- Administrator resolves issue (unlocks account, resets password, etc.)
- User returns to login with corrected credentials
- Continue with step 1

## Exception Flows

### 2a: Account Locked Due to Failed Attempts
**If** this authentication attempt would exceed maximum failed attempts
- System locks account automatically
- System displays: "Account temporarily locked due to multiple failed login attempts. Please try again in 30 minutes or contact your administrator."
- System logs account lockout event
- System sends notification email to user (optional)
- User must wait for automatic unlock or contact administrator
- No retry is allowed until unlock occurs

### 2b: Account Administratively Disabled
**If** account has been disabled by administrator
- System displays: "Account has been disabled. Please contact your administrator."
- System logs attempted access to disabled account
- No retry is possible without administrator intervention
- User must contact support for resolution

### 2c: Missing Required Fields
**If** user submits form with empty user identifier or password
- System displays: "Please enter both user identifier and password."
- System highlights empty fields
- System positions cursor on first empty field
- User fills in missing information
- Continue with step 9

### 2d: Network or System Error
**If** system cannot complete authentication due to technical issue
- System displays: "Unable to complete login. Please try again in a few moments."
- System logs technical error details
- System allows immediate retry
- If problem persists after 3 attempts, suggest contacting support

## Business Rules Applied
- **Maximum Failed Attempts**: Account locks after 5 consecutive failed login attempts (configurable)
- **Lockout Duration**: Locked accounts automatically unlock after 30 minutes (configurable)
- **Credential Case Insensitivity**: User identifiers and passwords processed case-insensitively
- **Error Message Specificity**: Error messages are helpful but don't reveal security details (e.g., don't confirm which part was wrong for non-existent users)
- **Audit Logging**: All failed authentication attempts logged for security analysis

## Data Captured/Changed
**Read**:
- User security profile (to validate credentials)
- Failed attempt counter (to check lockout threshold)
- Account status (to check if enabled/disabled/locked)

**Written**:
- Failed authentication log entry (user, timestamp, error type, IP address)
- Failed attempt counter (incremented on each failure)
- Account status (changed to locked if threshold exceeded)
- Lockout timestamp (if account locked)

**Reset**:
- Failed attempt counter (reset to 0 on successful authentication)

## Acceptance Criteria

**Given** user enters incorrect password  
**When** authentication fails  
**Then** system displays password error message and allows retry

**Given** user enters non-existent user identifier  
**When** authentication fails  
**Then** system displays user not found message and allows retry

**Given** authentication fails for any reason  
**When** error is displayed  
**Then** password field is cleared and user identifier is retained (unless user not found error)

**Given** user fails authentication multiple times  
**When** failure count is displayed  
**Then** count shows current attempt and maximum allowed

**Given** user on 3rd or 4th failed attempt  
**When** error is displayed  
**Then** "Forgot Password?" link is prominently shown

**Given** user exceeds maximum failed attempts  
**When** 5th failure occurs  
**Then** account is locked and lockout message is displayed

**Given** locked account  
**When** 30 minutes have passed  
**Then** account is automatically unlocked and user can attempt login

**Given** any authentication failure  
**When** failure occurs  
**Then** failure event is logged with details

**Given** successful authentication after previous failures  
**When** login succeeds  
**Then** failed attempt counter is reset to 0

## UI/UX Considerations
- **Error Message Clarity**: Messages clearly explain what went wrong and how to fix it
- **Progressive Disclosure**: Show "Forgot Password?" link more prominently after 2+ failures
- **Visual Feedback**: Error messages in red, warnings in yellow, with appropriate icons
- **Attempt Counter**: Show remaining attempts to help user understand situation
- **Field Focus**: Cursor automatically positioned in field that needs correction
- **Field Retention**: Keep user identifier populated to reduce re-entry
- **Help Access**: Easy access to password reset and support contact
- **Accessibility**: Error messages announced to screen readers
- **Mobile Friendly**: Touch-friendly error recovery on mobile devices

## Security Considerations
- Failed attempts logged with timestamp, IP address, and user agent
- Account lockout prevents brute force attacks
- Error messages don't reveal whether user exists (security best practice)
- Password field always cleared on failure (prevents shoulder surfing)
- Lockout notifications may be sent to user email for awareness
- Failed attempt counter resistant to manipulation
- Rate limiting prevents rapid-fire attempts
- Lockout events trigger security monitoring alerts

## Performance Requirements
- Authentication validation completes within 2 seconds even on failure
- Failed attempt counter updates immediately
- Account lockout activates instantly upon threshold
- Error messages display without noticeable delay

## Definition of Done
- [x] Users receive specific, helpful error messages on authentication failure
- [x] Password field is cleared on failure for security
- [x] User identifier retained on password errors for convenience
- [x] Cursor positioned appropriately based on error type
- [x] Failed attempt counter increments correctly
- [x] Account locks after maximum failed attempts
- [x] Locked accounts automatically unlock after specified time
- [x] "Forgot Password?" link prominently available after multiple failures
- [x] All failure events are logged with details
- [x] Error messages are accessible and mobile-friendly
- [x] Performance requirements met

## Related Use Cases
- **UC-001**: User Login (primary flow this use case recovers from)
- **UC-006**: Password Reset (alternative when user can't remember password)
- **UC-007**: Account Unlock (administrator manually unlocks account)

## Source References
- **COBOL Program**: COSGN00C (error handling routines)
- **Business Requirement**: BR-001 (User Authentication - FR-001.4)
- **Screen**: COSGN00 (error message display area)
