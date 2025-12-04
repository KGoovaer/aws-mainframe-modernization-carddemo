# POC Architecture: Account Management (MOD-002)

**Module**: Account Management  
**Business Requirements**: BR-002-account-management.md  
**POC Focus**: Validate core account inquiry and update operations  
**Date**: 2025-11-24

## Overview

This POC implements the core account management functionality using simplified patterns to validate business logic feasibility. The implementation focuses on:

1. **Account Inquiry** (FR-002.1) - View account and customer details
2. **Account/Customer Update** (FR-002.2) - Update account and customer information transactionally

**Out of Scope for POC**:
- Interest calculation (FR-002.3) - Complex batch processing, defer to later
- Billing cycle management (FR-002.4) - Batch processing, defer to later
- Data export (FR-002.5) - Utility function, not core validation

## POC Architecture Pattern

### Simple 3-Layer Architecture

```
┌─────────────────────────────────────────────────────┐
│                 Presentation Layer                   │
│  AccountController - REST API endpoints             │
└────────────────────┬────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────┐
│                 Business Logic Layer                 │
│  AccountService - Business rules & validation       │
│  CustomerService - Customer operations              │
└────────────────────┬────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────┐
│                  Data Access Layer                   │
│  AccountRepository (Spring Data JPA)                │
│  CustomerRepository (Spring Data JPA)               │
│  H2 Database (embedded, file-based)                 │
└─────────────────────────────────────────────────────┘
```

### Technology Stack

- **Java 21** (LTS)
- **Spring Boot 3.3.x**
- **Spring Data JPA** - Data access with repositories
- **H2 Database** - Embedded file-based database
- **JUnit 5 + AssertJ** - Testing
- **Maven** - Build management

### No Complex Patterns

**What we're NOT using** (for POC simplicity):
- ❌ CQRS or MediatR
- ❌ Domain events
- ❌ Axon Framework
- ❌ Complex repository implementations
- ❌ Azure/AWS cloud services
- ❌ Message queuing
- ❌ Advanced DDD patterns

**What we ARE using** (for POC validation):
- ✅ Simple service layer
- ✅ Spring Data JPA repositories
- ✅ JPA entities with relationships
- ✅ Transactional integrity with @Transactional
- ✅ Basic validation
- ✅ REST controllers

## Data Model (POC)

### JPA Entities

#### Account Entity
Maps to COBOL `CVACT01Y.cpy` - Account record

```java
@Entity
@Table(name = "accounts")
public class Account {
    @Id
    @Column(length = 11)
    private String accountId;           // ACCT-ID PIC 9(11)
    
    @Column(length = 9)
    private String customerId;          // Customer FK
    
    @Column(length = 1)
    private String activeStatus;        // ACCT-ACTIVE-STATUS PIC X(01)
    
    @Column(precision = 12, scale = 2)
    private BigDecimal currentBalance;  // ACCT-CURR-BAL PIC S9(10)V99
    
    @Column(precision = 12, scale = 2)
    private BigDecimal creditLimit;     // ACCT-CREDIT-LIMIT PIC S9(10)V99
    
    @Column(precision = 12, scale = 2)
    private BigDecimal cashCreditLimit; // ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
    
    private LocalDate openDate;         // ACCT-OPEN-DATE PIC X(10)
    private LocalDate expirationDate;   // ACCT-EXPIRAION-DATE PIC X(10)
    private LocalDate reissueDate;      // ACCT-REISSUE-DATE PIC X(10)
    
    @Column(precision = 12, scale = 2)
    private BigDecimal currentCycleCredit; // ACCT-CURR-CYC-CREDIT
    
    @Column(precision = 12, scale = 2)
    private BigDecimal currentCycleDebit;  // ACCT-CURR-CYC-DEBIT
    
    @Column(length = 10)
    private String addressZip;          // ACCT-ADDR-ZIP PIC X(10)
    
    @Column(length = 10)
    private String groupId;             // ACCT-GROUP-ID PIC X(10)
    
    // Relationships
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "customerId", insertable = false, updatable = false)
    private Customer customer;
    
    // Getters, setters
}
```

#### Customer Entity
Maps to COBOL `CVCUS01Y.cpy` - Customer record

```java
@Entity
@Table(name = "customers")
public class Customer {
    @Id
    @Column(length = 9)
    private String customerId;           // CUST-ID PIC 9(09)
    
    @Column(length = 25)
    private String firstName;            // CUST-FIRST-NAME PIC X(25)
    
    @Column(length = 25)
    private String middleName;           // CUST-MIDDLE-NAME PIC X(25)
    
    @Column(length = 25)
    private String lastName;             // CUST-LAST-NAME PIC X(25)
    
    @Column(length = 50)
    private String addressLine1;         // CUST-ADDR-LINE-1
    
    @Column(length = 50)
    private String addressLine2;         // CUST-ADDR-LINE-2
    
    @Column(length = 50)
    private String addressLine3;         // CUST-ADDR-LINE-3
    
    @Column(length = 2)
    private String stateCode;            // CUST-ADDR-STATE-CD
    
    @Column(length = 3)
    private String countryCode;          // CUST-ADDR-COUNTRY-CD
    
    @Column(length = 10)
    private String zip;                  // CUST-ADDR-ZIP
    
    @Column(length = 15)
    private String phoneNumber1;         // CUST-PHONE-NUM-1
    
    @Column(length = 15)
    private String phoneNumber2;         // CUST-PHONE-NUM-2
    
    @Column(length = 9)
    private String ssn;                  // CUST-SSN PIC 9(09)
    
    @Column(length = 20)
    private String governmentId;         // CUST-GOVT-ISSUED-ID
    
    private LocalDate dateOfBirth;       // CUST-DOB-YYYY-MM-DD
    
    @Column(length = 10)
    private String eftAccountId;         // CUST-EFT-ACCOUNT-ID
    
    @Column(length = 1)
    private String primaryCardHolder;    // CUST-PRI-CARD-HOLDER-IND
    
    private Integer ficoScore;           // CUST-FICO-CREDIT-SCORE PIC 9(03)
    
    // Relationships
    @OneToMany(mappedBy = "customer")
    private List<Account> accounts;
    
    // Getters, setters
}
```

### Database Schema (H2)

Spring Data JPA will auto-create tables based on entities:

- `accounts` table - Account entity
- `customers` table - Customer entity
- Foreign key: `accounts.customerId` → `customers.customerId`

## Service Layer Design

### AccountService

**Responsibility**: Account operations with business rules

```java
@Service
@Transactional
public class AccountService {
    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    
    // Account inquiry
    public AccountDetailsDto getAccountDetails(String accountId) {
        // 1. Fetch account
        // 2. Fetch customer (via FK)
        // 3. Build response DTO
        // Validates FR-002.1
    }
    
    // Account update (with customer)
    public void updateAccountAndCustomer(
            UpdateAccountRequest accountRequest,
            UpdateCustomerRequest customerRequest) {
        // 1. Validate account exists
        // 2. Validate customer exists
        // 3. Validate business rules (credit limit, dates)
        // 4. Update account
        // 5. Update customer
        // 6. Save both (transactional)
        // Validates FR-002.2 & Rule 002-4 (transactional integrity)
    }
}
```

**Business Rules Enforced**:
- Rule 002-1: Account number format (11 digits)
- Rule 002-2: Credit limit ≥ current balance
- Rule 002-3: Date logical relationships
- Rule 002-4: Transactional integrity (both update or neither)
- Rule 002-7: FICO score range (300-850)

### CustomerService

**Responsibility**: Customer-specific operations

```java
@Service
@Transactional(readOnly = true)
public class CustomerService {
    private final CustomerRepository customerRepository;
    
    public CustomerDto getCustomer(String customerId) {
        // Fetch customer details
    }
}
```

## REST API Design

### Endpoints

#### GET /api/accounts/{accountId}
**Purpose**: Retrieve account details with customer information

**Response**:
```json
{
  "accountId": "00000000001",
  "customerId": "000000001",
  "activeStatus": "Y",
  "currentBalance": 1250.50,
  "creditLimit": 5000.00,
  "cashCreditLimit": 1000.00,
  "openDate": "2024-01-15",
  "expirationDate": "2027-01-15",
  "customer": {
    "customerId": "000000001",
    "firstName": "John",
    "lastName": "Doe",
    "addressLine1": "123 Main St",
    "city": "Springfield",
    "stateCode": "IL",
    "zip": "62701",
    "phoneNumber1": "555-1234",
    "ficoScore": 720
  }
}
```

#### PUT /api/accounts/{accountId}
**Purpose**: Update account and customer information

**Request**:
```json
{
  "account": {
    "creditLimit": 7500.00,
    "cashCreditLimit": 1500.00,
    "activeStatus": "Y",
    "expirationDate": "2028-01-15"
  },
  "customer": {
    "addressLine1": "456 Oak Ave",
    "phoneNumber1": "555-5678",
    "ficoScore": 750
  }
}
```

**Response**: 200 OK or 400 Bad Request with validation errors

## Validation Strategy

### Field-Level Validation (DTO)

```java
public record UpdateAccountRequest(
    @DecimalMin("0.01")
    @DecimalMax("999999999.99")
    BigDecimal creditLimit,
    
    @DecimalMin("0.00")
    @DecimalMax("999999999.99")
    BigDecimal cashCreditLimit,
    
    @Pattern(regexp = "[YN]")
    String activeStatus,
    
    @Future
    LocalDate expirationDate
) {}
```

### Business Rule Validation (Service)

```java
// In AccountService.updateAccountAndCustomer()

// Rule 002-2: Credit limit ≥ current balance
if (newCreditLimit.compareTo(account.getCurrentBalance()) < 0) {
    throw new ValidationException(
        "Credit limit cannot be less than current balance");
}

// Rule 002-3: Expiration date > open date
if (newExpirationDate.isBefore(account.getOpenDate())) {
    throw new ValidationException(
        "Expiration date must be after open date");
}

// Rule 002-7: FICO score range
if (ficoScore < 300 || ficoScore > 850) {
    throw new ValidationException(
        "FICO score must be between 300 and 850");
}
```

## Testing Strategy

### Unit Tests (Service Layer)

```java
@DataJpaTest
@Import({AccountService.class, CustomerService.class})
class AccountServiceTest {
    
    @Test
    void getAccountDetails_existingAccount_returnsDetails() {
        // Test FR-002.1 account inquiry
    }
    
    @Test
    void updateAccountAndCustomer_validData_updatesSuccessfully() {
        // Test FR-002.2 update with transactional integrity
    }
    
    @Test
    void updateAccountAndCustomer_creditLimitBelowBalance_throwsException() {
        // Test Rule 002-2: credit limit validation
    }
    
    @Test
    void updateAccountAndCustomer_invalidDates_throwsException() {
        // Test Rule 002-3: date validation
    }
    
    @Test
    void updateAccountAndCustomer_invalidFicoScore_throwsException() {
        // Test Rule 002-7: FICO score range
    }
    
    @Test
    void updateAccountAndCustomer_accountFails_customerNotUpdated() {
        // Test Rule 002-4: transactional integrity
        // Update account fails → customer should not be updated
    }
}
```

### Test Coverage Goals

- **Happy path**: Account inquiry and update succeed
- **Business rules**: All validation rules tested
- **Transactional integrity**: Rollback on partial failure
- **Edge cases**: Boundary values for FICO, credit limits, dates

## Sample Data

### Sample Accounts
```sql
INSERT INTO customers VALUES 
('000000001', 'John', 'A', 'Doe', '123 Main St', '', '', 'IL', 'USA', 
 '62701', '555-1234', '', '123456789', 'DL12345', '1980-05-15', 
 'EFT001', 'Y', 720);

INSERT INTO accounts VALUES 
('00000000001', '000000001', 'Y', 1250.50, 5000.00, 1000.00, 
 '2024-01-15', '2027-01-15', NULL, 0.00, 0.00, '62701', 'STANDARD');
```

## POC Success Criteria

The POC is successful if:

- ✅ Can retrieve account with customer details (FR-002.1)
- ✅ Can update account and customer together (FR-002.2)
- ✅ Credit limit validation enforced (Rule 002-2)
- ✅ Date validation enforced (Rule 002-3)
- ✅ Transactional integrity maintained (Rule 002-4)
- ✅ FICO score validation enforced (Rule 002-7)
- ✅ REST API endpoints functional
- ✅ Unit tests passing (>80% coverage)
- ✅ Can demonstrate working feature via API/UI

## Known POC Limitations

**Not implemented** (out of POC scope):
- ❌ Interest calculation (complex batch logic)
- ❌ Billing cycle management (batch processing)
- ❌ Data export utilities
- ❌ Card association (CARDXREF lookup) - simplified FK relationship
- ❌ Transaction category balances - defer to transaction module
- ❌ Optimistic concurrency detection (Rule 002-5) - simplified for POC
- ❌ Production security (JWT, authorization)
- ❌ Audit logging
- ❌ Advanced error handling

**Simplified for POC**:
- Using simple FK relationship instead of CARDXREF lookup
- Basic transactional integrity without optimistic locking
- Single-table updates instead of multi-file VSAM coordination
- H2 database instead of production Azure SQL/PostgreSQL

## File Structure

```
src/poc/carddemo-poc/
├── src/main/java/com/carddemo/poc/
│   ├── entity/
│   │   ├── Account.java
│   │   └── Customer.java
│   ├── repository/
│   │   ├── AccountRepository.java
│   │   └── CustomerRepository.java
│   ├── service/
│   │   ├── AccountService.java
│   │   └── CustomerService.java
│   ├── controller/
│   │   └── AccountController.java
│   └── dto/
│       ├── AccountDetailsDto.java
│       ├── UpdateAccountRequest.java
│       └── UpdateCustomerRequest.java
├── src/main/resources/
│   ├── application.properties
│   └── data.sql (sample data)
└── src/test/java/com/carddemo/poc/service/
    └── AccountServiceTest.java
```

## Next Steps After POC

If POC is successful:
1. **Validate** business logic matches COBOL behavior
2. **Demonstrate** to stakeholders
3. **Document** findings and any business rule adjustments
4. **Plan** production implementation with:
   - CQRS pattern
   - Optimistic concurrency
   - Audit logging
   - Production database
   - Security hardening

---

**This POC architecture prioritizes simplicity and rapid validation over production-readiness.**
