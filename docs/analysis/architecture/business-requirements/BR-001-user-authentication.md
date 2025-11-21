# BR-001: User Authentication

## Business Context

User authentication is the entry point and security gateway for the entire CardDemo application. Every user must successfully authenticate before accessing any system functionality. The authentication system validates user credentials, establishes user identity and role, and directs users to appropriate application areas based on their authorization level. This capability is critical for:

- **Security**: Ensuring only authorized personnel access sensitive financial data
- **Compliance**: Meeting regulatory requirements for access control and audit trails
- **User Experience**: Providing seamless entry to the application
- **Access Control**: Establishing user identity for all subsequent operations

## Functional Requirements

### FR-001.1: User Credential Validation
**Description**: System must validate user credentials against stored user security data.

**User Need**: Users need secure access to the system while preventing unauthorized access to sensitive financial information.

**Source**: COSGN00C (lines 200-350), USRSEC file read operations

**Details**:
- System accepts user identifier and password
- System validates both fields are provided
- System performs case-insensitive credential matching
- System confirms user exists in user security database
- System verifies password matches stored credentials

### FR-001.2: Role-Based Access Differentiation
**Description**: System must recognize different user roles and route users to appropriate application areas.

**User Need**: Administrative users require access to different functions than regular users, necessitating role-based routing.

**Source**: COSGN00C (lines 280-310), COMMAREA initialization with user type

**Details**:
- System identifies user role (Administrator or Regular User)
- System stores user role in session context
- System routes administrators to administrative functions
- System routes regular users to standard customer-facing functions
- User role remains consistent throughout user session

### FR-001.3: Session Initialization
**Description**: System must establish and maintain user session context upon successful authentication.

**User Need**: Users need their identity and preferences maintained as they navigate through the application.

**Source**: COSGN00C (COMMAREA initialization), COCOM01Y copybook

**Details**:
- System creates session context with user identifier
- System stores user role in session
- System tracks entry point for navigation
- Session context persists across all user interactions
- Session context is available to all application functions

### FR-001.4: Authentication Failure Handling
**Description**: System must provide clear feedback when authentication fails and allow retry.

**User Need**: Users need to understand why access was denied and have opportunity to correct errors.

**Source**: COSGN00C (error handling routines, lines 350-450)

**Details**:
- System detects missing user identifier
- System detects missing password
- System detects non-existent user
- System detects incorrect password
- System displays specific error messages for each failure type
- System allows user to retry authentication
- System maintains screen focus for user convenience

### FR-001.5: Voluntary Session Termination
**Description**: Users must be able to cancel authentication and exit the system.

**User Need**: Users need ability to abandon login attempt without consequence.

**Source**: COSGN00C (F3 key handling)

**Details**:
- System provides cancel option during authentication
- System displays confirmation message on voluntary exit
- System terminates session cleanly
- No partial authentication state is retained

## Business Rules

### Rule 001: Credential Case Insensitivity
**Statement**: User identifiers and passwords are processed in case-insensitive manner.

**Rationale**: Improves user experience by reducing authentication failures due to case variations. Mainframe tradition where uppercase was standard.

**Source**: COSGN00C (lines 240-250 - uppercase conversion)

### Rule 002: Mandatory Authentication Fields
**Statement**: Both user identifier and password must be provided before authentication can proceed.

**Rationale**: Ensures complete credential set for security validation. Prevents partial authentication attempts.

**Source**: COSGN00C (validation logic, lines 220-235)

### Rule 003: User Type Determines Initial Navigation
**Statement**: Upon successful authentication, user type ('A' for Admin, 'U' for User) determines which application area is presented first.

**Rationale**: Different user roles have different primary responsibilities and require access to different functionality sets.

**Source**: COSGN00C (routing logic, lines 280-310), USRSEC file user type field

### Rule 004: Authentication Required for All Access
**Statement**: All application functionality requires successful authentication as prerequisite.

**Rationale**: Security requirement - no access to sensitive financial data without verified identity.

**Source**: COSGN00C as mandatory entry point (transaction CC00), application architecture

### Rule 005: Single Active Session Per User
**Statement**: Each authentication creates independent session context.

**Rationale**: Maintains isolation between concurrent user sessions for security and data integrity.

**Source**: COMMAREA initialization pattern, CICS session management

## Data Requirements

### Entity: User Security Profile
**Purpose**: Stores user credentials and authorization information for authentication and access control.

**Key Attributes**:
- **User Identifier**: Unique identifier for each user (8 characters)
- **Password**: User's authentication credential (8 characters)
- **User Type**: Role designation ('A' = Administrator, 'U' = Regular User)
- **Additional Profile Data**: May include name, contact information, preferences

**Relationships**:
- One user profile per authorized system user
- User profile links to user's business transactions and activities
- User type determines accessible functionality

**Source**: USRSEC file, CSUSR01Y copybook structure

### Entity: Session Context
**Purpose**: Maintains user identity and state throughout application usage.

**Key Attributes**:
- **Current User Identifier**: Identity of authenticated user
- **User Type**: Role for authorization decisions
- **Entry Point**: Where user entered current function
- **Navigation Context**: Supporting back/forward navigation
- **Current Program**: Active function identifier

**Relationships**:
- One session context per active user session
- Session context passed to all application functions
- Session context destroyed on logout or timeout

**Source**: COCOM01Y copybook (COMMAREA structure)

## User Personas Affected

### Administrative Staff
Administrative users are responsible for system configuration, user management, and oversight of operational functions. They require:
- Access to authentication system to reach administrative functions
- Clear routing to administrative menu after login
- Same security validation as other users
- Ability to manage other users (separate from authentication itself)

### Customer Service Representatives
Regular users who handle customer inquiries, account management, and transactions. They require:
- Quick, efficient authentication to begin serving customers
- Routing to customer service functions
- Clear error messages to resolve login issues quickly
- Reliable session establishment for day-long usage

### System Auditors
While not directly logging in differently, auditors need:
- Authentication events to be logged for compliance
- Ability to trace who accessed what information
- Failed authentication attempts tracked for security analysis

## Non-Functional Requirements

### Performance
- **Response Time**: Authentication validation completes within 2 seconds
- **Concurrent Users**: System supports multiple simultaneous authentication attempts
- **Availability**: Authentication service available during all business hours (99.9% uptime)

### Security
- **Credential Protection**: Passwords not displayed during entry
- **Secure Storage**: Credentials stored using industry-standard encryption/hashing
- **Audit Trail**: All authentication attempts (success and failure) logged with timestamp and source
- **Session Security**: Session context protected from unauthorized access
- **Timeout**: Inactive sessions automatically terminated after defined period
- **Account Lockout**: Repeated failed authentication attempts trigger temporary account lockout

### Usability
- **Clear Feedback**: Users receive specific, actionable error messages
- **Intuitive Interface**: Login interface is simple and self-explanatory
- **Accessibility**: Authentication interface meets accessibility standards
- **Mobile Support**: Authentication works on various device types
- **Help Access**: Users can access help or password reset from login screen

### Compliance
- **Access Control**: Meets SOX requirements for access to financial systems
- **Audit Requirements**: Authentication events logged per PCI-DSS requirements
- **Data Privacy**: User credential handling complies with data protection regulations
- **Retention**: Authentication logs retained per regulatory requirements

## Success Criteria

- [x] Authorized users can successfully authenticate with valid credentials
- [x] Authentication failures provide specific, helpful error messages
- [x] Users are automatically routed to appropriate application area based on role
- [x] User identity and role are maintained throughout application session
- [x] Unauthorized access attempts are blocked and logged
- [x] Users can abandon authentication attempt without creating partial session
- [x] Authentication completes within acceptable timeframe (< 2 seconds)
- [x] Authentication supports concurrent users without degradation
- [x] All authentication events are logged for audit purposes
- [x] Password credentials are protected during entry and storage

## Dependencies

### Upstream Dependencies
- User security database must be populated with authorized users
- User management function must create/maintain user profiles
- System infrastructure (web server, database) must be operational

### Downstream Dependencies
- All application functions depend on authentication establishing user identity
- Authorization decisions throughout application rely on authenticated user type
- Audit trails require authenticated user identifier
- User preferences and personalization depend on user identity

## Migration Notes

**Current Implementation (COBOL/CICS)**:
- COSGN00C program with BMS screen COSGN00
- USRSEC VSAM file for user storage
- Passwords stored as uppercase text (not hashed)
- COMMAREA-based session context
- Function key navigation (F3 = exit)

**Modern Implementation Approach**:
- Web-based authentication form
- Spring Security or OAuth2/OIDC
- Secure password hashing (bcrypt, PBKDF2)
- JWT tokens or secure cookie sessions
- RESTful API for authentication service
- SQL database for user storage
- Multi-factor authentication capability
- Password reset self-service
- "Remember me" functionality
- Single sign-on (SSO) integration potential

**Security Enhancements Required**:
- Replace plaintext passwords with hashed passwords
- Implement account lockout after failed attempts
- Add password complexity requirements
- Implement session timeout
- Add MFA support
- HTTPS/TLS mandatory
- Comprehensive audit logging

## Related Documents

- **Use Cases**: UC-001 (User Login), UC-002 (User Logout), UC-003 (Authentication Failure Recovery)
- **User Stories**: US-001 through US-012 (authentication feature set)
- **COBOL Analysis**: PROG-COSGN00C.md, SCREEN-COSGN00.md
- **Data Structures**: COPY-COCOM01Y.md (session context), COPY-CSUSR01Y.md (user data)
