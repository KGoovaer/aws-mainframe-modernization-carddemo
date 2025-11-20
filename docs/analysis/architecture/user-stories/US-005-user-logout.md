# US-005: User Logout

## User Story
**As a** customer service representative  
**I want to** log out of the application when I'm done working or leaving my desk  
**So that** no one else can access customer data using my credentials

## Source
**COBOL Program**: COSGN00C (F3 key exit handling)  
**Business Requirement**: BR-001 (User Authentication - FR-001.5)  
**Use Case**: UC-002 (User Logout)

## Acceptance Criteria

**Given** I am logged into the application  
**When** I click the logout button from any page  
**Then** I am logged out and redirected to the login page

**Given** I click logout  
**When** logout completes  
**Then** my session is fully invalidated and I cannot use the back button to access secured pages

**Given** I log out  
**When** logout completes  
**Then** a logout success message is displayed briefly before redirect to login

**Given** I log out  
**When** logout completes  
**Then** the logout event is logged with my username, timestamp, and duration of session

**Given** I am logged in and have multiple application tabs open  
**When** I log out from one tab  
**Then** all other tabs are also logged out and redirect to the login page

**Given** I have successfully logged out  
**When** I attempt to access any application URL directly  
**Then** I am redirected to the login page

## Business Rules
- Logout fully invalidates session server-side
- Session data is cleared from client (cookies, storage)
- Logout is always successful (no failure mode)
- Logout available on every application page
- All active tabs/windows logged out simultaneously

## UI/UX Considerations
- Logout button/link prominently displayed (typically top-right)
- Logout button clearly labeled and easily accessible
- Optional confirmation dialog if user has unsaved changes
- Brief success message: "You have been logged out successfully"
- Automatic redirect to login page after 2-3 seconds
- Accessible logout option (keyboard navigable)

## Security Considerations
- Session token invalidated server-side immediately
- Client-side session data cleared (cookies, localStorage)
- Back button navigation prevented after logout
- Session cannot be resumed without re-authentication
- Logout event logged for audit trail
- Multi-tab/window session termination

## Technical Notes
- Session invalidation happens server-side
- Client receives confirmation of invalidation
- Session ID removed from active session store
- Client-side cleanup includes all session storage
- Redirect uses replace (not push) to prevent back button access

## Definition of Done
- [x] User can logout from any application page
- [x] Logout button is prominently displayed and accessible
- [x] Session is fully invalidated server-side on logout
- [x] Client-side session data is cleared
- [x] User is redirected to login page
- [x] Back button does not provide access after logout
- [x] Logout event is logged with details
- [x] Multiple tabs handle logout correctly
- [x] Success message displayed before redirect
- [x] Logout completes within 1 second
