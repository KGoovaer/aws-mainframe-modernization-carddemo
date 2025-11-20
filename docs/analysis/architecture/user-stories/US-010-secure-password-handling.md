# US-010: Secure Password Handling

## User Story
**As a** user of the CardDemo application  
**I want** my password to be securely stored and transmitted  
**So that** my account cannot be compromised even if data is intercepted or breached

## Source
**COBOL Program**: COSGN00C (password handling - enhancement from plaintext)  
**Business Requirement**: BR-001 (Non-Functional Requirements - Security)  
**Use Case**: UC-001 (User Login - security considerations)

## Acceptance Criteria

**Given** I am entering my password on the login page  
**When** I type characters in the password field  
**Then** the characters are masked/hidden (displayed as dots or asterisks)

**Given** I submit the login form with my password  
**When** the form is transmitted to the server  
**Then** the transmission occurs over HTTPS (encrypted connection)

**Given** my password needs to be stored in the database  
**When** password is stored  
**Then** it is hashed using a strong algorithm (bcrypt, PBKDF2, or Argon2) and never stored as plaintext

**Given** I authenticate with my password  
**When** password comparison occurs  
**Then** the system compares hashed values (never compares plaintext)

**Given** a database breach occurs  
**When** attacker accesses user data  
**Then** passwords are protected by strong hashing and cannot be reversed to plaintext

**Given** password hashing is performed  
**When** hash is created  
**Then** unique salt is used per password (prevents rainbow table attacks)

## Business Rules
- Passwords never stored in plaintext or reversible encryption
- Password transmission only over HTTPS
- Password hashing uses industry-standard algorithms
- Salt unique per password for security
- Password never logged or displayed in any system output

## UI/UX Considerations
- Password field clearly marked with eye icon
- Optional "show password" toggle for user convenience
- Password masked by default for security
- Visual confirmation that connection is secure (HTTPS padlock)
- Password field autocomplete handled securely

## Security Considerations
- **Hashing Algorithm**: bcrypt (preferred), PBKDF2, or Argon2
- **Salt**: Unique random salt per password
- **Iterations**: High iteration count for key stretching (e.g., 100,000+ for PBKDF2)
- **HTTPS**: Mandatory for all authentication traffic
- **No Logging**: Passwords never appear in logs, error messages, or debug output
- **Memory Security**: Password cleared from memory after use
- **Hash Upgrade**: Plan to upgrade hashing algorithm as security standards evolve

## Technical Notes
- Password hashing occurs server-side (not client-side)
- Hash stored in user security database
- Verification compares hash of entered password with stored hash
- Failed comparisons take same time as successful (prevents timing attacks)
- Password complexity requirements enforced at creation (separate user story)

## Modern Implementation (Migration from COBOL)
**Current (COBOL/VSAM)**:
- Password stored as uppercase plaintext in USRSEC file
- Password transmitted over TN3270 (limited encryption)
- Simple uppercase string comparison

**Target (Modern .NET)**:
- Password hashed with bcrypt or PBKDF2
- Password transmitted over HTTPS/TLS 1.3
- Secure hash comparison using constant-time comparison
- SQL database with proper access controls

## Definition of Done
- [x] Password field masks input characters
- [x] Password transmitted over HTTPS only
- [x] Password stored as strong hash (bcrypt/PBKDF2/Argon2)
- [x] Unique salt used per password
- [x] Password never stored in plaintext
- [x] Password never logged or exposed
- [x] Hash comparison uses constant-time comparison
- [x] Security best practices followed
- [x] Migration plan from plaintext to hashed passwords documented
