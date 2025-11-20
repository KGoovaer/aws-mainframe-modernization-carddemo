# US-008: Role-Based Login Routing

## User Story
**As an** administrative user  
**I want to** be automatically directed to the administrative dashboard after login  
**So that** I can immediately access the administrative functions I need

## Source
**COBOL Program**: COSGN00C (routing logic, lines 280-310)  
**Business Requirement**: BR-001 (User Authentication - FR-001.2)  
**Use Case**: UC-001 (User Login - Main Success Scenario step 9)

## Acceptance Criteria

**Given** I am an administrative user (user type 'A')  
**When** I successfully authenticate  
**Then** I am automatically routed to the administrative dashboard

**Given** I am a regular user (user type 'U')  
**When** I successfully authenticate  
**Then** I am automatically routed to the main customer service menu

**Given** I am successfully authenticated  
**When** routing occurs  
**Then** my user role is stored in my session context

**Given** I am successfully authenticated  
**When** I navigate to different application areas  
**Then** my user role determines which functions I can access

**Given** my user type changes (e.g., promoted to admin)  
**When** I log in after the change  
**Then** I am routed to the appropriate landing page for my new role

## Business Rules
- User type determines initial landing page (Rule 003)
- User role persists throughout session
- Role-based routing happens automatically (no user choice)
- Role determines available menu options and functions

## UI/UX Considerations
- Routing happens automatically and quickly (< 500ms)
- Optional: brief loading message "Loading your dashboard..."
- Landing page appropriate to user's role and needs
- No manual selection required (reduces clicks)
- Consistent routing behavior

## Security Considerations
- User role validated from authoritative source (user database)
- Role cannot be manipulated by client
- Role stored securely in session
- Role verified for each privileged action
- Role changes require re-authentication

## Technical Notes
- User type retrieved from user security profile during authentication
- User type stored in session context (user ID, user type, session ID)
- Routing logic on server-side (not client-side)
- Session middleware provides role to all requests
- Authorization checks use role throughout application

## Definition of Done
- [x] Administrative users routed to admin dashboard
- [x] Regular users routed to main menu
- [x] Routing happens automatically after authentication
- [x] User role stored in session context
- [x] Routing completes quickly (< 500ms)
- [x] Role cannot be manipulated by client
- [x] Role determines accessible functions throughout session
- [x] Role changes reflected on next login
