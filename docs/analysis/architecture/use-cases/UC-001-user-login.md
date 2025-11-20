# UC-001: User Login

## Overview
**Actor**: Customer Service Representative, Administrative Staff  
**Goal**: Authenticate and gain access to the CardDemo application  
**Frequency**: Multiple times daily per user (start of shift, after break, after timeout)  
**Priority**: Critical - Required for all system access

## Preconditions
- User has been provisioned with valid user account
- User knows their assigned user identifier and password
- System is available and operational
- User has access to web browser or application client

## Main Success Scenario
1. User navigates to application login page
2. System displays login form with user identifier and password fields
3. User enters their user identifier
4. User enters their password (displayed as masked characters)
5. User submits the login form
6. System validates credentials against user security database
7. System creates session context with user identity and role
8. System displays success confirmation
9. System routes user to appropriate landing page:
   - Administrative users → Administrative Dashboard
   - Regular users → Main Application Menu
10. User begins working with application features

## Alternative Flows

### 2a: User Accesses Remembered Credentials
**If** user previously selected "remember me" option
- System pre-fills user identifier
- User only enters password
- Continue with step 5

### 5a: User Cancels Login
**If** user clicks cancel or close
- System abandons authentication attempt
- System displays confirmation message
- System returns to login page or closes application
- No session is created

### 6a: Session Already Exists
**If** user already has active session from another tab/window
- System detects existing session
- System offers to continue existing session or create new one
- **If** continue existing: redirect to current location in existing session
- **If** create new: invalidate old session and create new one

## Exception Flows

### 6b: User Identifier Not Found
**If** entered user identifier does not exist in system
- System displays error message: "User identifier not recognized. Please verify and try again."
- System clears password field
- System keeps cursor focus on user identifier field
- User can retry from step 3

### 6c: Password Incorrect
**If** entered password does not match stored password for user
- System displays error message: "Password incorrect. Please try again."
- System clears password field
- System keeps cursor focus on password field
- System increments failed attempt counter
- User can retry from step 4

### 6d: Account Locked
**If** user account has been locked due to too many failed attempts
- System displays error message: "Account temporarily locked due to multiple failed login attempts. Please contact your administrator or try again in 30 minutes."
- System does not allow authentication attempt
- System logs lockout event
- User must wait for automatic unlock or contact administrator

### 6e: Account Disabled
**If** user account has been administratively disabled
- System displays error message: "Account has been disabled. Please contact your administrator."
- System does not allow authentication attempt
- System logs attempted access to disabled account
- User must contact administrator to resolve

### 6f: System Unavailable
**If** authentication service or user database is unavailable
- System displays error message: "Authentication service temporarily unavailable. Please try again in a few moments."
- System logs system availability issue
- User can retry after brief wait

### 6g: Network Timeout
**If** authentication request times out
- System displays error message: "Request timed out. Please check your connection and try again."
- User can retry from step 5

## Business Rules Applied
- **Rule 001**: Credential Case Insensitivity - User identifiers and passwords processed case-insensitively (referenced from BR-001)
- **Rule 002**: Mandatory Authentication Fields - Both fields required (referenced from BR-001)
- **Rule 003**: User Type Determines Initial Navigation - Routing based on role (referenced from BR-001)
- **Rule 004**: Authentication Required for All Access - No bypass mechanism (referenced from BR-001)

## Data Captured/Changed
**Read**:
- User security profile (user identifier, password hash, user type, account status)

**Written**:
- Authentication audit log (user, timestamp, result, IP address)
- Session record (user identifier, user type, session ID, creation timestamp)
- Failed attempt counter (on authentication failure)

## Acceptance Criteria

**Given** a user with valid credentials  
**When** user enters correct user identifier and password  
**Then** user is authenticated and routed to appropriate application area based on role

**Given** an administrative user successfully authenticates  
**When** authentication completes  
**Then** user is routed to Administrative Dashboard

**Given** a regular user successfully authenticates  
**When** authentication completes  
**Then** user is routed to Main Application Menu

**Given** a user with non-existent user identifier  
**When** user attempts to login  
**Then** system displays "User identifier not recognized" error and allows retry

**Given** a user with valid user identifier but wrong password  
**When** user attempts to login  
**Then** system displays "Password incorrect" error and allows retry

**Given** a user whose account is locked  
**When** user attempts to login  
**Then** system displays account locked message and prevents authentication

**Given** successful authentication  
**When** session is created  
**Then** user identity and role are maintained for all subsequent requests

**Given** any authentication attempt  
**When** attempt completes (success or failure)  
**Then** event is logged with user, timestamp, result, and source

## UI/UX Considerations
- **Simplicity**: Login form should be clean and uncluttered
- **Clarity**: Field labels clearly indicate required information
- **Feedback**: Password field masks input for security
- **Error Messages**: Specific but not revealing (don't indicate if user vs password was wrong for security)
- **Focus Management**: Cursor automatically positioned in first empty field
- **Loading State**: Show progress indicator during authentication
- **Accessibility**: Form meets WCAG 2.1 AA standards
- **Mobile Responsive**: Works on phones, tablets, and desktops
- **Branding**: Login page reflects CardDemo branding and professionalism

## Security Considerations
- Password field masked during entry
- Failed attempts logged and monitored
- Account lockout after threshold of failed attempts
- Session tokens securely generated and stored
- HTTPS required for all authentication traffic
- Authentication logs protected and retained
- No sensitive information in error messages
- Rate limiting to prevent brute force attacks

## Performance Requirements
- Authentication completes within 2 seconds under normal load
- System supports 100+ concurrent authentication requests
- Session creation completes in < 500ms
- Database query optimization for user lookup

## Definition of Done
- [x] User can successfully authenticate with valid credentials
- [x] Users are routed to correct landing page based on role
- [x] Invalid credentials result in appropriate error messages
- [x] Account lockout activates after failed attempt threshold
- [x] All authentication attempts are logged
- [x] Session context is properly established
- [x] Password is masked during entry
- [x] Form is accessible and mobile-responsive
- [x] Performance meets < 2 second requirement
- [x] Security requirements satisfied (HTTPS, logging, rate limiting)

## Related Use Cases
- **UC-002**: User Logout (complementary - end session)
- **UC-003**: Password Reset (alternative when password forgotten)
- **UC-004**: Session Timeout (automatic logout after inactivity)

## Source References
- **COBOL Program**: COSGN00C (authentication entry point)
- **Screen**: COSGN00 (sign-on screen)
- **Business Requirement**: BR-001 (User Authentication)
- **Data Structure**: COCOM01Y (session context), CSUSR01Y (user data)
