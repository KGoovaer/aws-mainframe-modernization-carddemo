# ADR-003: CQRS with MediatR for Application Layer

**Status**: Accepted  
**Date**: 2025-11-20  
**Deciders**: Software Architect  
**Consulted**: Development Team

## Context

The Application layer orchestrates business logic by handling commands (write operations) and queries (read operations). We need to decide how to structure this layer and what patterns/libraries to use for request handling, validation, and cross-cutting concerns.

### Key Considerations

1. **Separation of Concerns**: Read operations have different requirements than write operations
2. **Validation**: All commands and queries need input validation
3. **Cross-Cutting Concerns**: Logging, transactions, performance monitoring needed across all operations
4. **Testability**: Application layer must be easily unit testable
5. **Maintainability**: Clear patterns for developers to follow
6. **Scalability**: Read operations may need different optimization than writes

### Options Considered

#### Option 1: CQRS with MediatR
**Description**: Command Query Responsibility Segregation using MediatR library for in-process messaging.

**Implementation**:
```csharp
// Command
public record CreateAccountCommand(string CustomerId, decimal CreditLimit) 
    : IRequest<Result<AccountDto>>;

// Command Handler
public class CreateAccountCommandHandler 
    : IRequestHandler<CreateAccountCommand, Result<AccountDto>>
{
    public async Task<Result<AccountDto>> Handle(
        CreateAccountCommand request, CancellationToken cancellationToken)
    {
        // Business logic
    }
}

// Query
public record GetAccountQuery(string AccountId) : IRequest<AccountDto>;

// Query Handler
public class GetAccountQueryHandler : IRequestHandler<GetAccountQuery, AccountDto>
{
    public async Task<AccountDto> Handle(
        GetAccountQuery request, CancellationToken cancellationToken)
    {
        // Query logic
    }
}
```

**Pros**:
- Clear separation: Commands modify state, Queries read data
- Single responsibility: One handler per command/query
- Pipeline behaviors: Validation, logging, transactions as cross-cutting concerns
- Testability: Easy to unit test handlers in isolation
- Decoupling: Controllers don't reference handlers directly
- Industry standard: MediatR is de facto standard in .NET community
- Scalability: Can optimize read/write models separately

**Cons**:
- Additional abstraction layer (learning curve)
- Magic strings avoided but indirection can be confusing initially
- Small performance overhead (in-process messaging)

#### Option 2: Traditional Service Layer
**Description**: Application services with methods for each operation.

**Implementation**:
```csharp
public class AccountService
{
    public async Task<AccountDto> CreateAccount(CreateAccountRequest request);
    public async Task<AccountDto> GetAccount(string accountId);
    public async Task UpdateAccount(UpdateAccountRequest request);
    public async Task DeleteAccount(string accountId);
}
```

**Pros**:
- Simple, familiar pattern
- No additional libraries
- Direct method calls

**Cons**:
- Services tend to grow large (many methods)
- Difficult to apply cross-cutting concerns consistently
- Harder to test (mock entire service)
- No clear command/query separation
- Validation logic scattered

#### Option 3: Vertical Slice Architecture
**Description**: Each feature is a vertical slice with its own handler, no shared services.

**Pros**:
- Complete feature isolation
- Easy to add new features without affecting existing

**Cons**:
- Code duplication across slices
- No consistent patterns for cross-cutting concerns
- Difficult to enforce consistency

## Decision

**We will use CQRS with MediatR** for the Application layer with the following implementation:

### Command Structure
```csharp
// Command definition
public record CreateAccountCommand(
    string CustomerId,
    decimal CreditLimit,
    string AccountGroupId) : IRequest<Result<Guid>>;

// Validator (FluentValidation)
public class CreateAccountCommandValidator : AbstractValidator<CreateAccountCommand>
{
    public CreateAccountCommandValidator()
    {
        RuleFor(x => x.CustomerId).NotEmpty().MaximumLength(11);
        RuleFor(x => x.CreditLimit).GreaterThan(0).LessThanOrEqualTo(100000);
    }
}

// Handler
public class CreateAccountCommandHandler 
    : IRequestHandler<CreateAccountCommand, Result<Guid>>
{
    private readonly IAccountRepository _repository;
    private readonly IUnitOfWork _unitOfWork;
    
    public async Task<Result<Guid>> Handle(
        CreateAccountCommand request, 
        CancellationToken cancellationToken)
    {
        // 1. Create domain entity
        var account = Account.Create(
            CustomerId.From(request.CustomerId),
            Money.From(request.CreditLimit),
            request.AccountGroupId);
        
        // 2. Persist
        await _repository.AddAsync(account, cancellationToken);
        await _unitOfWork.SaveChangesAsync(cancellationToken);
        
        // 3. Return result
        return Result<Guid>.Success(account.Id.Value);
    }
}
```

### Query Structure
```csharp
// Query definition
public record GetAccountQuery(string AccountId) : IRequest<AccountDto>;

// Handler
public class GetAccountQueryHandler : IRequestHandler<GetAccountQuery, AccountDto>
{
    private readonly IAccountRepository _repository;
    private readonly IMapper _mapper;
    
    public async Task<AccountDto> Handle(
        GetAccountQuery request, 
        CancellationToken cancellationToken)
    {
        var account = await _repository.GetByIdAsync(
            AccountId.From(request.AccountId), 
            cancellationToken);
        
        return _mapper.Map<AccountDto>(account);
    }
}
```

### MediatR Pipeline Behaviors

**ValidationBehavior** (run for all requests):
```csharp
public class ValidationBehavior<TRequest, TResponse> 
    : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly IEnumerable<IValidator<TRequest>> _validators;
    
    public async Task<TResponse> Handle(
        TRequest request,
        RequestHandlerDelegate<TResponse> next,
        CancellationToken cancellationToken)
    {
        var context = new ValidationContext<TRequest>(request);
        var failures = _validators
            .Select(v => v.Validate(context))
            .SelectMany(result => result.Errors)
            .Where(f => f != null)
            .ToList();
        
        if (failures.Any())
            throw new ValidationException(failures);
        
        return await next();
    }
}
```

**LoggingBehavior** (run for all requests):
```csharp
public class LoggingBehavior<TRequest, TResponse> 
    : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly ILogger<LoggingBehavior<TRequest, TResponse>> _logger;
    
    public async Task<TResponse> Handle(
        TRequest request,
        RequestHandlerDelegate<TResponse> next,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation("Handling {RequestName}", typeof(TRequest).Name);
        var response = await next();
        _logger.LogInformation("Handled {RequestName}", typeof(TRequest).Name);
        return response;
    }
}
```

**TransactionBehavior** (run for commands only):
```csharp
public class TransactionBehavior<TRequest, TResponse> 
    : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly IUnitOfWork _unitOfWork;
    
    public async Task<TResponse> Handle(
        TRequest request,
        RequestHandlerDelegate<TResponse> next,
        CancellationToken cancellationToken)
    {
        if (IsQuery(request))
            return await next();
        
        // Commands wrapped in transaction
        using var transaction = await _unitOfWork.BeginTransactionAsync(cancellationToken);
        try
        {
            var response = await next();
            await transaction.CommitAsync(cancellationToken);
            return response;
        }
        catch
        {
            await transaction.RollbackAsync(cancellationToken);
            throw;
        }
    }
    
    private bool IsQuery(TRequest request) => 
        request.GetType().Name.EndsWith("Query");
}
```

### Controller Integration
```csharp
[ApiController]
[Route("api/[controller]")]
public class AccountsController : ControllerBase
{
    private readonly IMediator _mediator;
    
    public AccountsController(IMediator mediator) => _mediator = mediator;
    
    [HttpPost]
    public async Task<IActionResult> CreateAccount(
        [FromBody] CreateAccountRequest request,
        CancellationToken cancellationToken)
    {
        var command = new CreateAccountCommand(
            request.CustomerId, 
            request.CreditLimit,
            request.AccountGroupId);
        
        var result = await _mediator.Send(command, cancellationToken);
        
        return result.IsSuccess 
            ? CreatedAtAction(nameof(GetAccount), new { id = result.Value }, result.Value)
            : BadRequest(result.Error);
    }
    
    [HttpGet("{id}")]
    public async Task<IActionResult> GetAccount(
        string id, 
        CancellationToken cancellationToken)
    {
        var query = new GetAccountQuery(id);
        var account = await _mediator.Send(query, cancellationToken);
        return Ok(account);
    }
}
```

## Rationale

### Why CQRS?

1. **Separation of Concerns**: Read and write models have different requirements
   - **Writes**: Business logic, validation, domain events, transactional
   - **Reads**: Optimized queries, projections, caching, read-only

2. **Scalability**: Can optimize reads and writes independently
   - Cache query results
   - Use read replicas for queries
   - Separate read/write databases (future option)

3. **Maintainability**: 
   - One handler per operation (single responsibility)
   - Easy to find code (convention-based naming)
   - Clear intent (CreateAccountCommand vs. UpdateAccountCommand)

4. **Testability**: 
   - Unit test handlers in isolation
   - Mock repository dependencies easily
   - Test validators independently

### Why MediatR?

1. **Industry Standard**: De facto library for CQRS in .NET
2. **Pipeline Behaviors**: Apply cross-cutting concerns consistently (validation, logging, transactions)
3. **Decoupling**: Controllers don't reference handlers directly (loose coupling)
4. **Extensibility**: Easy to add behaviors (caching, performance monitoring)
5. **Community Support**: Active development, extensive documentation, large community

### Command/Query Naming Convention

- **Commands**: `{Verb}{Entity}Command` (CreateAccountCommand, UpdateCardCommand)
- **Queries**: `Get{Entity}Query`, `List{Entities}Query` (GetAccountQuery, ListTransactionsQuery)
- **Handlers**: `{CommandOrQuery}Handler` (CreateAccountCommandHandler)
- **Validators**: `{Command}Validator` (CreateAccountCommandValidator)

### Result Pattern

Use `Result<T>` for commands to handle success/failure without exceptions:

```csharp
public class Result<T>
{
    public bool IsSuccess { get; }
    public T Value { get; }
    public string Error { get; }
    
    public static Result<T> Success(T value) => new(true, value, null);
    public static Result<T> Failure(string error) => new(false, default, error);
}
```

Benefits:
- Explicit error handling
- Avoid exceptions for expected failures
- Railway-oriented programming

## Consequences

### Positive

- **Clear Structure**: Developers know where to put code (handlers, validators)
- **Consistent Patterns**: All operations follow same structure
- **Cross-Cutting Concerns**: Pipeline behaviors handle logging, validation, transactions automatically
- **Testability**: Easy to unit test handlers (mock dependencies, no controller concerns)
- **Decoupling**: Controllers are thin, no business logic
- **Scalability**: Can optimize read/write models separately
- **Maintainability**: One class per operation (single responsibility)

### Negative

- **Learning Curve**: Team needs to understand MediatR and CQRS
- **Indirection**: Extra layer between controller and business logic
- **Boilerplate**: More files (command, handler, validator per operation)
- **Performance**: Small overhead from in-process messaging (negligible for CardDemo)

### Mitigations

- **Documentation**: Provide examples and guidelines for common scenarios
- **Training**: Onboard team with CQRS/MediatR patterns
- **Templates**: Create file templates for commands/queries/handlers
- **Code Reviews**: Ensure patterns followed consistently

## Validation

We will validate this decision by:

1. **Developer Feedback**: Survey team after 3 months (is pattern helping or hindering?)
2. **Code Quality**: Review for consistency and adherence to patterns
3. **Test Coverage**: Verify high test coverage (>80%) due to improved testability
4. **Performance**: Monitor API response times (should be acceptable)

## Related Decisions

- **ADR-001**: Use Modular Monolith over Microservices
- **ADR-008**: Clean Architecture with DDD Tactical Patterns

## References

- [MediatR Documentation](https://github.com/jbogard/MediatR)
- [CQRS Pattern - Martin Fowler](https://martinfowler.com/bliki/CQRS.html)
- [Implementing CQRS in .NET - Microsoft](https://learn.microsoft.com/en-us/dotnet/architecture/microservices/microservice-ddd-cqrs-patterns/apply-simplified-microservice-cqrs-ddd-patterns)
- [Clean Architecture with MediatR - Jason Taylor](https://jasontaylor.dev/clean-architecture-getting-started/)
