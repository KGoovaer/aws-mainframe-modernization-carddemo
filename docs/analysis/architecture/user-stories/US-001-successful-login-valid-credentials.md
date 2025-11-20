# US-001: Successful Login with Valid Credentials

## User Story
**As a** customer service representative  
**I want to** log into the CardDemo application with my username and password  
**So that** I can access customer accounts and perform my daily work

## Source
**COBOL Program**: COSGN00C (authentication flow, lines 200-350)  
**Business Requirement**: BR-001 (User Authentication - FR-001.1)  
**Use Case**: UC-001 (User Login - Main Success Scenario)

## Acceptance Criteria

**Given** I have a valid user account with username "CSR001" and correct password  
**When** I enter my username and password on the login page and submit  
**Then** I am successfully authenticated and see the main application menu

**Given** I am a regular user (user type 'U')  
**When** I successfully log in  
**Then** I am directed to the main customer service menu

**Given** I am an administrative user (user type 'A')  
**When** I successfully log in  
**Then** I am directed to the administrative dashboard

**Given** I enter my username in mixed case (e.g., "csr001")  
**When** I submit the login form  
**Then** authentication succeeds because matching is case-insensitive

**Given** I successfully log in  
**When** authentication completes  
**Then** my login attempt is logged with timestamp, username, and result

## Business Rules
- Credential matching is case-insensitive (Rule 001)
- User type determines initial navigation destination (Rule 003)
- All authentication attempts must be logged (audit requirement)

## UI/UX Considerations
- Login form has username and password fields clearly labeled
- Password field masks input characters
- Submit button clearly labeled "Login" or "Sign In"
- Form is centered and visually appealing
- Loading indicator appears during authentication
- Form is accessible via keyboard (tab navigation)

## Technical Notes
- Session token created upon successful authentication
- Session includes user ID and user type
- Redirect to appropriate landing page based on user role
- Client receives session cookie or JWT token

## Definition of Done
- [x] User can enter credentials and submit login form
- [x] Valid credentials result in successful authentication
- [x] User is redirected to role-appropriate landing page
- [x] Session is properly established with user context
- [x] Login event is logged
- [x] Case-insensitive matching works correctly
- [x] Password is masked during entry
- [x] Form meets accessibility standards
