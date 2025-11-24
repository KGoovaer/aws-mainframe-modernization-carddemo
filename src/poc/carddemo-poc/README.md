# CardDemo POC - Java Spring Boot

## Overview

This is a Proof of Concept (POC) implementation of the CardDemo mainframe application using Java 21 and Spring Boot 3.x. The goal is to rapidly validate business logic and data models with minimal complexity.

**Key Characteristics**:
- ✅ Simple 3-layer architecture (Presentation → Business → Data)
- ✅ H2 file-based database (zero configuration)
- ✅ Spring Data JPA repositories (no custom implementations)
- ✅ REST API for Angular frontend
- ✅ Runs entirely locally (`mvn spring-boot:run`)

**Not Included** (for production version):
- ❌ CQRS or complex patterns
- ❌ Domain events or messaging
- ❌ Cloud services (Azure/AWS)
- ❌ Advanced error handling
- ❌ Comprehensive security

## Prerequisites

- Java 21 or higher
- (Optional) Node.js 18+ for Angular frontend

**Note**: Maven is NOT required! This project includes the Maven Wrapper (`./mvnw`) which automatically downloads and uses the correct Maven version.

## Getting Started

### Quick Start (All Services)

```bash
# Navigate to POC directory
cd src/poc/carddemo-poc

# Start both backend and frontend
./start-all.sh
```

This will open separate terminals for backend and frontend services.

### Manual Start

**Backend (Terminal 1):**
```bash
cd src/poc/carddemo-poc

# Run with Maven Wrapper (no Maven installation needed!)
./mvnw spring-boot:run

# Or use convenience script
./start-poc.sh
```

Backend will start on http://localhost:8080

**Frontend (Terminal 2):**
```bash
cd src/poc/carddemo-poc

# Run Angular (auto-installs dependencies)
./start-frontend.sh

# Or manually
cd frontend
npm install  # First time only
npm start
```

Frontend will start on http://localhost:4200

## Access Points

- **Angular UI**: http://localhost:4200
- **REST API**: http://localhost:8080/api
- **H2 Console**: http://localhost:8080/h2-console
  - JDBC URL: `jdbc:h2:file:./data/carddemo`
  - Username: `sa`
  - Password: (leave empty)

## Project Structure

```
carddemo-poc/
├── src/main/java/com/carddemo/poc/
│   ├── controller/           # REST controllers
│   ├── service/              # Business logic services
│   ├── repository/           # Spring Data JPA repositories
│   ├── entity/               # JPA entities
│   ├── dto/                  # Data Transfer Objects
│   └── CardDemoPocApplication.java
├── src/main/resources/
│   ├── application.properties
│   └── data.sql              # Sample data
├── src/test/java/com/carddemo/poc/
│   └── service/              # Unit tests
├── pom.xml
└── README.md
```

## Implemented Features

### ✅ MOD-001: Authentication (Backend + Frontend)
**Backend (Spring Boot)**:
- User login with credential validation
- Role-based routing (Admin vs User)
- Session management
- Error handling for invalid credentials
- REST API endpoints

**Frontend (Angular 18)**:
- Login screen with terminal theme (COSGN00C)
- Main menu with role-based filtering (COMEN01C)
- User authentication service
- Session storage management
- Responsive CICS-style UI

## Testing

```bash
# Run all tests
./mvnw test

# Run specific test
./mvnw test -Dtest=AuthenticationServiceTest
```

## Development Notes

### Database

H2 database file is created at `./data/carddemo.mv.db` on first run. To reset database:

```bash
rm -rf data/
./mvnw spring-boot:run
```

### Sample Users

Default users are loaded from `data.sql`:

| Username | Password | Type | Description |
|----------|----------|------|-------------|
| ADMIN01  | ADMIN01  | A    | Administrator |
| USER01   | USER01   | U    | Regular user |

## Architecture Decisions

This POC uses:
- **Simple patterns**: Repository + Service Layer (no CQRS)
- **Spring Data JPA**: Automatic repository implementations
- **H2 Database**: File-based, embedded database
- **Direct validation**: Business rules in service methods
- **Basic error handling**: Exceptions with clear messages

For production architecture with CQRS, DDD, and cloud services, see `docs/architecture/`.

## Testing the Application

See **[QUICK-START.md](./QUICK-START.md)** for:
- Complete testing scenarios
- Step-by-step demo script
- Troubleshooting guide
- Available test users
- Browser DevTools tips

**Quick Test:**
1. Start services: `./start-all.sh`
2. Open http://localhost:4200
3. Login: ADMIN01 / ADMIN01
4. Navigate to main menu
5. Logout

## Related Documentation

- **Quick Start Guide**: [QUICK-START.md](./QUICK-START.md)
- **POC Summary**: [POC-IMPLEMENTATION-SUMMARY.md](./POC-IMPLEMENTATION-SUMMARY.md)
- **Frontend README**: [frontend/README.md](./frontend/README.md)
- **Business Requirements**: `docs/analysis/architecture/business-requirements/BR-001-user-authentication.md`
- **POC Architecture**: `docs/architecture/poc/overview.md`
- **Agent Pipeline**: `agent_pipeline.md`
