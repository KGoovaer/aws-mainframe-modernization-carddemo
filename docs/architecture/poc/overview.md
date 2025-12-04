# POC Architecture Overview: CardDemo Modernization

**Status**: Draft  
**Last Updated**: 2025-11-20  
**Purpose**: Rapid validation of business logic and data model  
**Mode**: POC (Proof of Concept)

## Executive Summary

This POC architecture provides a **simplified, rapid-validation approach** to modernizing the CardDemo COBOL application to Java Spring Boot. The goal is to prove that the core business logic works correctly with minimal infrastructure complexity.

**Key Characteristics**:
- ✅ **Local-first**: Runs entirely on developer machine (no cloud dependencies)
- ✅ **Simple patterns**: 3-layer architecture (Presentation → Business → Data)
- ✅ **Rapid development**: Minimal setup, quick to implement and test
- ✅ **Validation focus**: Prove business logic, not production readiness
- ⚠️ **Not production-ready**: H2 database, synchronous processing, no scaling

## POC vs. Final Architecture

| Aspect | POC Architecture | Final Architecture |
|--------|------------------|-------------------|
| **Database** | H2 (file-based) | Azure SQL Database |
| **Patterns** | 3-layer, Repository | Clean Architecture, CQRS, DDD |
| **Messaging** | None (synchronous) | Azure Service Bus |
| **Deployment** | Local (`dotnet run`) | Azure Container Apps |
| **Scalability** | Single instance | Horizontal scaling |
| **Observability** | Console logging | Application Insights |
| **Complexity** | Low | High |
| **Time to implement** | 1-2 weeks | 2-3 months |

## Architectural Style

**Pattern**: Simple 3-Layer Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Presentation Layer                        │
│  (Angular Pages + REST API Controllers)                      │
│  - Handle HTTP requests/responses                           │
│  - Display UI components                                    │
│  - Call business services                                   │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                   Business Logic Layer                       │
│  (Services)                                                  │
│  - Implement business rules                                 │
│  - Coordinate between presentation and data                 │
│  - Transaction management                                   │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                      Data Access Layer                       │
│  (EF Core + Repository Pattern)                             │
│  - Database operations (CRUD)                               │
│  - Entity mapping                                           │
│  - Query optimization                                       │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                      H2 Database                         │
│  - Local file-based storage                                 │
│  - No setup required                                        │
│  - Perfect for POC validation                               │
└─────────────────────────────────────────────────────────────┘
```

## Key Architectural Principles

### 1. **Simplicity Over Sophistication**
- Use the simplest pattern that could work
- Avoid over-engineering
- Focus on business logic validation, not scalability

### 2. **Rapid Validation**
- Quick setup (< 5 minutes)
- Fast implementation (days, not weeks)
- Easy to demonstrate to stakeholders

### 3. **Local Development**
- No cloud account needed
- No network dependencies
- Works offline

### 4. **Direct Business Logic Translation**
- Map COBOL logic directly to Java services
- Preserve business rules exactly as in COBOL
- Focus on correctness, not optimization

### 5. **Disposable Code Mindset**
- POC code may not go to production
- Learning exercise and validation tool
- Informs final architecture decisions

## Technology Stack

See `technology-stack.md` for complete details.

**Core Technologies**:
- **Java Spring Boot 10** (LTS) with Java 14
- **H2** with Spring Data JPA
- **Angular Server** for UI
- **xUnit** for testing
- **No CQRS, no messaging, no cloud services**

## Solution Structure

See `solution-structure.md` for complete details.

```
CardDemo.POC/
├── carddemo-poc/              # All-in-one web application
│   ├── Controllers/               # REST API endpoints
│   ├── Pages/                     # Angular pages
│   ├── Services/                  # Business logic
│   ├── Data/                      # EF Core + H2
│   └── Program.java
└── CardDemo.POC.Tests/            # Unit tests
```

## Core Components (POC Implementation)

### 1. Authentication Service
**COBOL Source**: COSGN00C  
**POC Implementation**: `AuthenticationService.java`

```
AuthenticationService
├── LoginAsync(username, password) → User
├── LogoutAsync(userId) → void
├── ValidateSessionAsync(userId) → bool
└── LockAccountAsync(userId) → void
```

**Business Logic**:
- Validate username/password against `USRSEC` table
- Create session record in `UserSession` table
- Track failed login attempts
- Lock account after 3 failures

**POC Simplifications**:
- Passwords hashed with BCrypt (not plaintext like COBOL)
- Sessions stored in database (not CICS temporary storage)
- No timeout handling (can add later)

### 2. Account Management Service
**COBOL Sources**: COACTVWC, COACTUPC, CBACT01C, CBACT04C  
**POC Implementation**: `AccountService.java`

```
AccountService
├── GetAccountDetailsAsync(accountId) → AccountDetails
├── UpdateAccountAsync(accountId, request) → Account
├── CalculateInterestAsync() → InterestCalculationResult
└── ExportAccountsAsync(format) → byte[]
```

**Business Logic**:
- Read account from `ACCTDAT` table
- Join with customer from `CUSTDAT` table
- Validate account status, credit limit
- Calculate interest using original COBOL formula
- Update balances with transaction integrity

**POC Simplifications**:
- Synchronous processing (no event-driven)
- Single database transaction (not distributed)
- Simple interest calculation (monthly batch can run on-demand)

### 3. Card Management Service
**COBOL Sources**: COCRDLIC, COCRDSLC, COCRDUPC  
**POC Implementation**: `CardService.java`

```
CardService
├── SearchCardsAsync(criteria) → List<Card>
├── GetCardDetailsAsync(cardNumber) → CardDetails
└── UpdateCardAsync(cardNumber, request) → Card
```

**Business Logic**:
- Search cards with pagination
- Validate card number format
- Check card status (Active, Expired, Stolen, etc.)
- Update card details with optimistic locking

**POC Simplifications**:
- In-memory pagination (not database cursor)
- Simple optimistic locking (EF Core concurrency token)

### 4. Transaction Service
**COBOL Sources**: COTRN00C, COTRN01C, COTRN02C, CBTRN02C  
**POC Implementation**: `TransactionService.java`

```
TransactionService
├── ListTransactionsAsync(accountId, page) → List<Transaction>
├── GetTransactionDetailsAsync(transactionId) → TransactionDetails
├── AddTransactionAsync(request) → Transaction
└── PostTransactionsAsync() → BatchResult
```

**Business Logic**:
- List transactions with pagination
- Add new transaction (validation + balance update)
- Post pending transactions (batch processing)
- Update category balances

**POC Simplifications**:
- Synchronous batch posting (not JCL job)
- Single-threaded processing
- No retry logic

## Data Model

See `docs/architecture/poc/data-model.md` for complete entity definitions.

**Core Entities**:
- `User` (USRSEC file)
- `Account` (ACCTDAT file)
- `Customer` (CUSTDAT file)
- `Card` (CARDDAT file)
- `Transaction` (TRANSACT file)
- `CategoryBalance` (TRANCATG file)

**Relationships**:
```
Customer 1───* Account 1───* Card
             │
             └───* Transaction
```

## API Endpoints (POC)

### Authentication API
```
POST   /api/auth/login          # User login
POST   /api/auth/logout         # User logout
GET    /api/auth/validate       # Validate session
```

### Account API
```
GET    /api/accounts/{id}                    # Get account details
PUT    /api/accounts/{id}                    # Update account
GET    /api/accounts/{id}/cards              # List cards for account
GET    /api/accounts/{id}/transactions       # List transactions
POST   /api/accounts/{id}/calculate-interest # Run interest calc
```

### Card API
```
GET    /api/cards                # Search cards
GET    /api/cards/{number}       # Get card details
PUT    /api/cards/{number}       # Update card
```

### Transaction API
```
GET    /api/transactions                    # List transactions
GET    /api/transactions/{id}               # Get transaction details
POST   /api/transactions                    # Add transaction
POST   /api/transactions/post-batch         # Post pending transactions
```

## UI Pages (Angular Server)

### User-Facing Pages
- `/login` - Login page (COSGN00)
- `/menu` - Main menu (COMEN01)
- `/accounts/{id}` - Account details (COACTVW)
- `/accounts/{id}/edit` - Update account (COACTUP)
- `/cards` - Card list (COCRDLI)
- `/cards/{number}` - Card details (COCRDSL)
- `/cards/{number}/edit` - Update card (COCRDUP)
- `/transactions` - Transaction list (COTRN00)
- `/transactions/{id}` - Transaction details (COTRN01)
- `/transactions/add` - Add transaction (COTRN02)

### Admin Pages
- `/admin/users` - User management (COUSR00-03)
- `/admin/reports` - Report generation (CORPT00)
- `/admin/billing` - Bill payment (COBIL00)

## Non-Functional Considerations (POC)

### Performance
- ⚠️ **Not optimized for scale**: Single-threaded, H2 limits
- ✅ **Fast enough for validation**: < 100ms response time for basic operations
- ⚠️ **No caching**: Direct database queries every time

### Security
- ✅ **Better than COBOL**: Hashed passwords (BCrypt), not plaintext
- ✅ **Session management**: Database-backed sessions
- ⚠️ **No HTTPS**: Local development only
- ⚠️ **No rate limiting**: Not needed for POC

### Reliability
- ⚠️ **No high availability**: Single instance
- ⚠️ **No retry logic**: Fail fast
- ⚠️ **No circuit breakers**: Direct calls only

### Observability
- ✅ **Console logging**: Structured logs to console
- ⚠️ **No telemetry**: Not tracking metrics
- ⚠️ **No distributed tracing**: Single process

## POC Validation Goals

### What POC Should Prove

✅ **Business Logic Correctness**:
- Authentication works (login/logout/lockout)
- Account operations match COBOL behavior
- Interest calculation produces same results
- Transaction posting logic is correct

✅ **Data Model Validity**:
- Entity relationships are correct
- Foreign keys are properly defined
- Data types match COBOL copybooks

✅ **User Experience**:
- UI flows match COBOL screens
- Navigation works as expected
- Error messages are clear

✅ **Feasibility**:
- COBOL logic can be translated to Java
- Performance is acceptable for business operations
- Development velocity is reasonable

### What POC Does NOT Prove

⚠️ **Production Readiness**:
- Scalability under load
- High availability and fault tolerance
- Cloud deployment and operations

⚠️ **Enterprise Integration**:
- Integration with other systems
- Event-driven architecture
- Asynchronous processing

⚠️ **Security Hardening**:
- Penetration testing
- Compliance (PCI-DSS, etc.)
- Advanced threat protection

⚠️ **Operational Concerns**:
- Monitoring and alerting
- Disaster recovery
- Multi-region deployment

## Migration from POC to Production

When POC validation is successful, the final architecture will:

1. **Replace H2 with Azure SQL Database**
2. **Implement Clean Architecture** (Domain, Application, Infrastructure layers)
3. **Add CQRS with MediatR** (separate read/write models)
4. **Introduce Domain Events** (event-driven architecture)
5. **Deploy to Azure Container Apps** (scalable, cloud-native)
6. **Add comprehensive observability** (Application Insights)
7. **Implement security hardening** (OAuth2, API keys, encryption)

See `docs/architecture/overview.md` for final production architecture.

## POC Development Workflow

### Phase 1: Setup (Day 1)
1. Create solution structure
2. Setup H2 database with EF Core
3. Create initial entity models
4. Implement basic authentication

### Phase 2: Core Services (Days 2-5)
1. Implement Account Service
2. Implement Card Service
3. Implement Transaction Service
4. Write unit tests for business logic

### Phase 3: UI Development (Days 6-8)
1. Create Angular pages for core flows
2. Implement authentication UI
3. Build account and card management pages
4. Add transaction pages

### Phase 4: Validation (Days 9-10)
1. Test all business logic against COBOL behavior
2. Run interest calculation and verify results
3. Test transaction posting batch
4. Demonstrate to stakeholders

## POC Success Criteria

### Technical Validation
- [ ] All authentication flows work correctly
- [ ] Account CRUD operations match COBOL behavior
- [ ] Interest calculation produces identical results to COBOL
- [ ] Transaction posting updates balances correctly
- [ ] Card management operations work
- [ ] Unit test coverage > 70%

### Business Validation
- [ ] Stakeholders can use the POC application
- [ ] UI flows match existing COBOL screens
- [ ] All business rules are enforced
- [ ] Error handling is clear and user-friendly

### Decision Criteria
- [ ] POC proves business logic can be translated
- [ ] Development velocity is acceptable (< 2 weeks)
- [ ] No showstopper technical issues discovered
- [ ] Stakeholders approve proceeding to final architecture

## Next Steps After POC

If POC is successful:

1. **Document Findings**: Lessons learned, complexity areas, risk mitigation
2. **Architecture Refinement**: Use POC insights to refine final architecture
3. **Begin Detailed Specification**: Create detailed specs for production implementation
4. **Plan Production Implementation**: Staffing, timeline, milestones
5. **Decommission POC**: Archive POC code, begin production development with final architecture

## Related Documents

- **POC Technology Stack**: `technology-stack.md`
- **POC Solution Structure**: `solution-structure.md`
- **POC Patterns**: `patterns/` directory
- **Final Architecture**: `../overview.md` (production target)
- **COBOL Analysis**: `../../analysis/cobol/` (source material)
- **Business Requirements**: `../../analysis/architecture/` (what to build)

---

**Remember**: POC is a validation tool, not the final product. Keep it simple, prove the concept, then build production-ready with final architecture.
