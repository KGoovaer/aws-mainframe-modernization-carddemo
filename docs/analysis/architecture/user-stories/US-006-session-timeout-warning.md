# US-006: Session Timeout Warning

## User Story
**As a** customer service representative  
**I want to** receive a warning before my session times out  
**So that** I can save my work and avoid losing progress

## Source
**Business Requirement**: BR-001 (Non-Functional Requirements - Security - Session timeout)  
**Use Case**: UC-004 (Session Timeout - Main Success Scenario steps 5-9)

## Acceptance Criteria

**Given** I am logged in and have been inactive for 25 minutes  
**When** the warning threshold is reached  
**Then** I see a warning notification stating "Your session will expire in 5 minutes due to inactivity"

**Given** I receive a session timeout warning  
**When** the warning is displayed  
**Then** I see a countdown timer showing minutes and seconds remaining

**Given** I see the session timeout warning  
**When** I click the "Stay Logged In" button  
**Then** my session is extended and the warning disappears

**Given** I see the session timeout warning  
**When** I perform any action in the application  
**Then** my session is automatically extended and the warning disappears

**Given** the timeout warning is displayed  
**When** I take no action  
**Then** the countdown continues until reaching zero

**Given** I have the application open in multiple tabs  
**When** timeout warning appears  
**Then** the warning is displayed in all tabs simultaneously

## Business Rules
- Warning appears 5 minutes before timeout (at 25 minutes of inactivity)
- Any user activity resets the timeout timer
- Warning is non-blocking (doesn't prevent continued work)
- Same timeout rules apply to all users (admin and regular)

## UI/UX Considerations
- Warning displayed as modal or toast notification
- Warning is attention-getting but not alarming
- Countdown timer clearly visible
- "Stay Logged In" button prominently displayed
- Option to dismiss warning (implies intent to continue working)
- Warning doesn't block application usage
- Accessible notification (screen reader announcement)
- Mobile-friendly warning display

## Security Considerations
- Warning is informational only (timeout enforced server-side)
- Client cannot extend timeout arbitrarily
- "Stay Logged In" sends heartbeat to server
- Server validates activity and grants extension
- Warning time not manipulable by client

## Technical Notes
- Client-side JavaScript tracks user activity
- Server-side tracks last activity timestamp
- "Stay Logged In" triggers heartbeat to server
- Server resets activity timestamp on heartbeat
- WebSocket or polling for real-time warning
- Countdown timer runs client-side

## Definition of Done
- [x] Warning displays 5 minutes before timeout
- [x] Warning shows countdown timer
- [x] "Stay Logged In" button extends session
- [x] Any application activity extends session automatically
- [x] Warning is non-blocking
- [x] Warning displays in all open tabs
- [x] Warning is accessible
- [x] Warning is mobile-friendly
- [x] Server enforces timeout server-side
- [x] Client cannot manipulate timeout
