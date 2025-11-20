# US-004: Account Lockout After Multiple Failed Attempts

## User Story
**As a** security administrator  
**I want** user accounts to be automatically locked after multiple failed login attempts  
**So that** the system is protected from brute force attacks and unauthorized access

## Source
**COBOL Program**: Security best practice (enhancement to COSGN00C)  
**Business Requirement**: BR-001 (Non-Functional Requirements - Security)  
**Use Case**: UC-003 (Authentication Failure Recovery - Exception Flow 2a)

## Acceptance Criteria

**Given** a user has failed to login 4 times consecutively  
**When** they fail a 5th time  
**Then** their account is automatically locked

**Given** an account has been locked due to failed attempts  
**When** the user attempts to login  
**Then** they see the message "Account temporarily locked due to multiple failed login attempts. Please try again in 30 minutes or contact your administrator."

**Given** an account has been locked  
**When** 30 minutes have passed since the lockout  
**Then** the account is automatically unlocked and failed attempt counter is reset

**Given** an account is locked  
**When** the lockout occurs  
**Then** the event is logged with username, timestamp, number of failed attempts, and last attempt IP

**Given** a user successfully logs in  
**When** authentication succeeds  
**Then** any previous failed attempt counter for that user is reset to 0

**Given** a locked account  
**When** the user tries to login during lockout period  
**Then** no authentication is attempted and lockout message is displayed

## Business Rules
- Maximum 5 consecutive failed attempts allowed
- Lockout duration is 30 minutes from time of lockout
- Successful authentication resets failed attempt counter
- Administrator can manually unlock account before auto-unlock
- Lockout applies to all login attempts (online and API)

## UI/UX Considerations
- Lockout message clearly explains reason and remediation
- Message includes estimated unlock time if possible
- Provides option to contact administrator
- Warning given after 3rd and 4th failed attempts about impending lockout
- Accessible error messaging

## Security Considerations
- Prevents brute force password attacks
- Lockout event triggers security monitoring alert
- Repeated lockouts for same user may indicate attack
- IP address logged with lockout for investigation
- Lockout notification may be sent to user's registered email
- Administrator can view lockout history

## Technical Notes
- Failed attempt counter stored server-side per user
- Counter increments atomically to prevent race conditions
- Lockout timestamp recorded for auto-unlock calculation
- Background job or scheduler handles auto-unlocks
- Lockout applies across all application instances

## Definition of Done
- [x] Account locks after 5 consecutive failed attempts
- [x] Locked account displays appropriate message
- [x] Locked account prevents authentication during lockout period
- [x] Account automatically unlocks after 30 minutes
- [x] Successful authentication resets failed attempt counter
- [x] Lockout events are logged with full details
- [x] Warning displayed on 3rd and 4th attempts
- [x] Administrator can manually unlock if needed
- [x] Security monitoring alerted on lockout
