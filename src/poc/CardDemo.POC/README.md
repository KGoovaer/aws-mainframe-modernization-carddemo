# CardDemo POC - Proof of Concept Implementation

This is a simplified proof-of-concept implementation for the CardDemo mainframe modernization project. The POC validates business logic and data models using straightforward .NET patterns.

## Technology Stack

- **.NET 10** (LTS) with C# 14
- **Blazor Server** for UI
- **ASP.NET Core** for API
- **Entity Framework Core 10** with SQLite
- **xUnit** for testing
- **Swagger** for API documentation

## Project Structure

```
src/poc/CardDemo.POC/
├── CardDemo.POC.Web/              # Main application
│   ├── Controllers/               # API controllers
│   ├── Pages/                     # Blazor pages
│   ├── Shared/                    # Shared Blazor components
│   ├── Services/                  # Business logic services
│   ├── Data/
│   │   ├── CardDemoDbContext.cs   # EF Core DbContext
│   │   └── Entities/              # EF Core entities
│   ├── wwwroot/                   # Static files
│   └── Program.cs                 # Application startup
└── CardDemo.POC.Tests/            # Unit tests
    └── Services/                  # Service tests
```

## Prerequisites

- [.NET 10 SDK](https://dotnet.microsoft.com/download/dotnet/10.0)
- A code editor (Visual Studio 2022, VS Code, or Rider)

## Getting Started

### 1. Restore Dependencies

```powershell
cd src\poc\CardDemo.POC\CardDemo.POC.Web
dotnet restore
```

### 2. Run the Application

```powershell
dotnet run
```

The application will start and be available at:
- **Blazor UI**: https://localhost:7001
- **Swagger API**: https://localhost:7001/swagger

### 3. Run Tests

```powershell
cd ..\CardDemo.POC.Tests
dotnet test
```

## Features

### Blazor UI
- **Home Page**: Overview of the application
- **Accounts Page**: Account management (ready for implementation)
- **Customers Page**: Customer management (ready for implementation)
- **Transactions Page**: Transaction management (ready for implementation)

### REST API
Available at `/swagger`:
- **GET /api/customers** - Get all customers
- **GET /api/customers/{id}** - Get customer by ID
- **POST /api/customers** - Create new customer
- **PUT /api/customers/{id}** - Update customer
- **DELETE /api/customers/{id}** - Delete customer
- **GET /api/accounts** - Get all accounts
- **GET /api/accounts/{id}** - Get account by ID
- **POST /api/accounts** - Create new account
- **PUT /api/accounts/{id}** - Update account
- **DELETE /api/accounts/{id}** - Delete account

### Database
- SQLite database (`carddemo.db`) is automatically created on first run
- Database is stored in the application directory
- Schema includes: Customers, Accounts, Cards, Transactions

## Architecture Pattern

This POC uses a **simple layered architecture**:

1. **Presentation Layer** (Controllers, Pages)
   - Blazor Server pages for UI
   - API controllers for REST endpoints

2. **Business Logic Layer** (Services)
   - Services use DbContext directly (no repository layer)
   - Business rules validation
   - Simple error handling

3. **Data Access Layer** (DbContext, Entities)
   - Entity Framework Core with SQLite
   - POCO entities
   - Basic relationships

## Key Simplifications for POC

This POC deliberately keeps things simple:
- ✅ DbContext used directly in services (no repository pattern)
- ✅ Basic validation and error handling
- ✅ In-memory SQLite for tests
- ✅ Minimal abstractions

**Not included** (production features):
- ❌ CQRS or MediatR
- ❌ Domain events or messaging
- ❌ Advanced error handling middleware
- ❌ Extensive validation frameworks
- ❌ Caching, retry policies, etc.

## Development Guidelines

### Adding New Features

1. **Add Entity**: Create in `Data/Entities/`
2. **Update DbContext**: Add DbSet and configure in `OnModelCreating`
3. **Create Service**: Add service class in `Services/`
4. **Register Service**: Add to `Program.cs`
5. **Create Controller**: Add API controller in `Controllers/`
6. **Add Tests**: Create test class in `Tests/Services/`
7. **Create UI**: Add Blazor page in `Pages/`

### Code Style
- Keep it simple and straightforward
- Use async/await for all database operations
- Add XML comments for public APIs
- Reference COBOL mappings in entity comments

## Testing

Tests use **SQLite in-memory database**:
- Fast and isolated
- No external dependencies
- Tests run in parallel

Run all tests:
```powershell
dotnet test
```

Run with detailed output:
```powershell
dotnet test --logger "console;verbosity=detailed"
```

## Troubleshooting

### Port Already in Use
Change the port in `Properties/launchSettings.json` or run with:
```powershell
dotnet run --urls "https://localhost:5001;http://localhost:5000"
```

### Database Issues
Delete `carddemo.db` file and restart the application to recreate it.

### Build Errors
Clean and rebuild:
```powershell
dotnet clean
dotnet build
```

## Next Steps

1. Implement account management features
2. Add customer CRUD operations
3. Implement transaction processing
4. Add validation rules from COBOL business logic
5. Create comprehensive test coverage

## Resources

- [.NET 10 Documentation](https://docs.microsoft.com/dotnet/)
- [Blazor Documentation](https://docs.microsoft.com/aspnet/core/blazor/)
- [Entity Framework Core](https://docs.microsoft.com/ef/core/)
- [Project Documentation](../../../docs/README.md)
