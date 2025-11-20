# UC-002: User Logout

## Overview
**Actor**: Customer Service Representative, Administrative Staff (any authenticated user)  
**Goal**: Terminate current session and securely exit the application  
**Frequency**: Once per session (end of shift, before break, when leaving workstation)  
**Priority**: High - Required for security and proper session management

## Preconditions
- User is authenticated and has active session
- User is viewing any application page/screen

## Main Success Scenario
1. User clicks logout button/link (available on all application pages)
2. System displays logout confirmation dialog
3. User confirms logout action
4. System invalidates current session token
5. System clears session context
6. System logs logout event (user, timestamp)
7. System displays logout confirmation message
8. System redirects user to login page
9. User sees fresh login page, ready for next authentication

## Alternative Flows

### 3a: User Cancels Logout
**If** user clicks cancel on confirmation dialog
- System abandons logout process
- User remains on current page with active session
- No session changes occur

### 4a: Logout Without Confirmation
**If** organization policy doesn't require confirmation
- Skip step 2 (confirmation dialog)
- Proceed directly to step 4 (invalidate session)
- Faster logout but no accidental click protection

### 8a: Logout from Specific High-Security Function
**If** user was in high-security function (e.g., user management, financial reports)
- System ensures any unsaved changes are handled before logout
- System may display additional warning if unsaved work detected
- Proceed to step 4 after user acknowledges

## Exception Flows

### 4b: Session Already Invalid
**If** session has already been invalidated (timeout, forced logout, duplicate login)
- System detects invalid session during logout attempt
- Skip session invalidation (already done)
- Display message: "Session has already ended"
- Redirect to login page
- Log duplicate logout attempt

### 4c: Logout Service Unavailable
**If** logout service temporarily unavailable
- System logs error condition
- System clears client-side session data regardless
- System displays message: "Logout completed locally. Session will expire automatically."
- User sees login page but server-side session may persist briefly

### 8b: Multiple Open Application Tabs
**If** user has application open in multiple browser tabs
- Logout from one tab invalidates session globally
- Other tabs detect invalid session on next interaction
- Other tabs redirect to login page automatically
- User sees consistent logout across all tabs

## Business Rules Applied
- **Session Security**: All logout actions must fully invalidate session
- **Audit Trail**: All voluntary logouts must be logged for audit purposes
- **Clean Termination**: No session remnants should remain after logout
- **Universal Availability**: Logout option available on every application page

## Data Captured/Changed
**Read**:
- Current session identifier
- User identifier from session

**Written**:
- Logout audit log (user, timestamp, source page, IP address)
- Session status (marked as terminated)

**Deleted**:
- Session token (invalidated)
- Client-side session data (cleared)

## Acceptance Criteria

**Given** an authenticated user on any application page  
**When** user clicks logout button  
**Then** user is prompted to confirm logout action

**Given** user confirms logout  
**When** logout completes  
**Then** session is fully invalidated and user sees login page

**Given** successful logout  
**When** user attempts to use back button or navigate to application URL  
**Then** user is redirected to login page and cannot access application without re-authenticating

**Given** user with multiple application tabs open  
**When** user logs out from one tab  
**Then** all other tabs become invalid and redirect to login on next interaction

**Given** any logout action  
**When** logout completes  
**Then** logout event is logged with user identifier, timestamp, and source

**Given** session has already expired  
**When** user clicks logout  
**Then** user sees appropriate message and is directed to login page without error

## UI/UX Considerations
- **Accessibility**: Logout button/link consistently positioned (typically top-right navigation)
- **Visibility**: Logout option clearly labeled and easy to find
- **Confirmation**: Optional confirmation prevents accidental logout
- **Feedback**: Clear visual confirmation that logout succeeded
- **Branding**: Logout confirmation page maintains application branding
- **Instructions**: Logout page may include "Login again" button for convenience
- **Unsaved Work Warning**: If user has unsaved changes, warn before logout

## Security Considerations
- Session token fully invalidated server-side (not just client)
- Session data cleared from client (cookies, local storage)
- Logout event logged for audit trail
- Automatic redirect prevents using back button to access secured pages
- Multi-tab/window session invalidation
- Session cleanup prevents session hijacking
- No sensitive data cached after logout

## Performance Requirements
- Logout completes within 1 second
- Session invalidation propagates to all application servers immediately
- Client-side cleanup completes instantly
- Redirect to login page is immediate after logout

## Definition of Done
- [x] User can successfully logout from any application page
- [x] Session is fully invalidated server-side
- [x] Client-side session data is cleared
- [x] User is redirected to login page
- [x] Logout event is logged with details
- [x] User cannot access application after logout without re-authenticating
- [x] Multiple tabs handle logout correctly
- [x] Back button does not allow access to secured pages
- [x] Logout completes within 1 second
- [x] Optional confirmation dialog works correctly

## Related Use Cases
- **UC-001**: User Login (complementary - create session)
- **UC-004**: Session Timeout (automatic logout after inactivity)
- **UC-005**: Forced Logout (administrator terminates user session)

## Source References
- **COBOL Program**: COSGN00C (F3 key handling for exit)
- **Business Requirement**: BR-001 (User Authentication - FR-001.5)
- **Data Structure**: COCOM01Y (session context - cleared on logout)
