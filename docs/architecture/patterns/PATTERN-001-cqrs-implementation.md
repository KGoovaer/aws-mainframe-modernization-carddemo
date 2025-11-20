# PATTERN-001: CQRS Implementation with MediatR

**Pattern Type**: Application Architecture  
**Category**: Request Handling, Application Layer  
**Applies To**: CardDemo.Application project  
**Last Updated**: 2025-11-20

## Intent

Implement Command Query Responsibility Segregation (CQRS) using MediatR to separate read and write operations in the Application layer, providing clear structure, testability, and scalability.

## Motivation

- **Separation of Concerns**: Read operations (queries) have different requirements than write operations (commands)
- **Single Responsibility**: Each command/query handler focuses on one operation
- **Cross-Cutting Concerns**: Apply validation, logging, transactions consistently across all operations
- **Testability**: Easy to unit test handlers in isolation
- **Scalability**: Optimize read and write models independently

## Structure

```
Application Layer (CardDemo.Application)
├── {Module}/
│   ├── Commands/
│   │   └── {Operation}/
│   │       ├── {Operation}Command.cs       # Request definition
│   │       ├── {Operation}CommandHandler.cs # Business logic
│   │       └── {Operation}CommandValidator.cs # Validation rules
│   └── Queries/
│       └── {Operation}/
│           ├── {Operation}Query.cs          # Request definition
│           ├── {Operation}QueryHandler.cs   # Query logic
│           └── {Operation}Dto.cs            # Response model
```

## Participants

### 1. Command (Request)
**Responsibility**: Define the intent to change system state

```csharp
public record CreateAccountCommand(
    string CustomerId,
    decimal CreditLimit,
    string AccountGroupId) : IRequest<Result<Guid>>;
```

**Characteristics**:
- Immutable (use `record` type)
- Contains only data (no behavior)
- Returns `Result<T>` for error handling
- Implements `IRequest<TResponse>` from MediatR

### 2. Command Handler
**Responsibility**: Execute business logic for the command

```csharp
public class CreateAccountCommandHandler 
    : IRequestHandler<CreateAccountCommand, Result<Guid>>
{
    private readonly IAccountRepository _repository;
    private readonly IUnitOfWork _unitOfWork;
    private readonly IEventPublisher _eventPublisher;
    private readonly ILogger<CreateAccountCommandHandler> _logger;
    
    public CreateAccountCommandHandler(
        IAccountRepository repository,
        IUnitOfWork unitOfWork,
        IEventPublisher eventPublisher,
        ILogger<CreateAccountCommandHandler> logger)
    {
        _repository = repository;
        _unitOfWork = unitOfWork;
        _eventPublisher = eventPublisher;
        _logger = logger;
    }
    
    public async Task<Result<Guid>> Handle(
        CreateAccountCommand request, 
        CancellationToken cancellationToken)
    {
        try
        {
            // 1. Create domain aggregate
            var account = Account.Create(
                CustomerId.From(request.CustomerId),
                Money.From(request.CreditLimit),
                request.AccountGroupId);
            
            // 2. Persist to repository
            await _repository.AddAsync(account, cancellationToken);
            await _unitOfWork.SaveChangesAsync(cancellationToken);
            
            // 3. Publish domain events (if any)
            await _eventPublisher.PublishAsync(
                new AccountCreatedEvent(account.Id), 
                cancellationToken);
            
            _logger.LogInformation(
                "Account {AccountId} created for customer {CustomerId}", 
                account.Id, 
                request.CustomerId);
            
            // 4. Return success result
            return Result<Guid>.Success(account.Id.Value);
        }
        catch (DomainException ex)
        {
            _logger.LogWarning(ex, "Domain validation failed for CreateAccountCommand");
            return Result<Guid>.Failure(ex.Message);
        }
    }
}
```

**Characteristics**:
- Single responsibility (one handler per command)
- Depends on abstractions (interfaces)
- Returns `Result<T>` for error handling
- Logs important events

### 3. Command Validator
**Responsibility**: Validate command data before execution

```csharp
public class CreateAccountCommandValidator : AbstractValidator<CreateAccountCommand>
{
    public CreateAccountCommandValidator()
    {
        RuleFor(x => x.CustomerId)
            .NotEmpty().WithMessage("Customer ID is required")
            .Length(11).WithMessage("Customer ID must be exactly 11 characters");
        
        RuleFor(x => x.CreditLimit)
            .GreaterThan(0).WithMessage("Credit limit must be greater than zero")
            .LessThanOrEqualTo(100000).WithMessage("Credit limit cannot exceed $100,000");
        
        RuleFor(x => x.AccountGroupId)
            .NotEmpty().WithMessage("Account group ID is required");
    }
}
```

**Characteristics**:
- Uses FluentValidation library
- Declarative rules
- Descriptive error messages
- Executes in validation pipeline behavior (before handler)

### 4. Query (Request)
**Responsibility**: Define the intent to read system state

```csharp
public record GetAccountQuery(string AccountId) : IRequest<AccountDto>;
```

**Characteristics**:
- Immutable (use `record` type)
- Contains only query parameters
- Returns DTO (data transfer object)
- Implements `IRequest<TResponse>` from MediatR

### 5. Query Handler
**Responsibility**: Retrieve and project data

```csharp
public class GetAccountQueryHandler : IRequestHandler<GetAccountQuery, AccountDto>
{
    private readonly IAccountRepository _repository;
    private readonly IMapper _mapper;
    private readonly ILogger<GetAccountQueryHandler> _logger;
    
    public GetAccountQueryHandler(
        IAccountRepository repository,
        IMapper mapper,
        ILogger<GetAccountQueryHandler> logger)
    {
        _repository = repository;
        _mapper = mapper;
        _logger = logger;
    }
    
    public async Task<AccountDto> Handle(
        GetAccountQuery request, 
        CancellationToken cancellationToken)
    {
        var accountId = AccountId.From(request.AccountId);
        var account = await _repository.GetByIdAsync(accountId, cancellationToken);
        
        if (account is null)
        {
            _logger.LogWarning("Account {AccountId} not found", request.AccountId);
            throw new NotFoundException(nameof(Account), request.AccountId);
        }
        
        return _mapper.Map<AccountDto>(account);
    }
}
```

**Characteristics**:
- Read-only operations (no state changes)
- Returns DTOs (not domain entities)
- Can use optimized queries (projections)
- Caching can be applied in pipeline behavior

### 6. DTO (Response)
**Responsibility**: Data transfer object for query results

```csharp
public record AccountDto(
    Guid Id,
    string AccountNumber,
    string CustomerName,
    decimal Balance,
    decimal CreditLimit,
    string Status,
    DateTime CreatedDate);
```

**Characteristics**:
- Immutable (use `record` type)
- Contains only data needed by client
- No business logic
- Can be cached

## Pipeline Behaviors

MediatR pipeline behaviors run for all requests, providing cross-cutting concerns.

### Validation Behavior
**Runs**: Before all commands and queries  
**Purpose**: Validate input using FluentValidation

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
        if (!_validators.Any())
            return await next();
        
        var context = new ValidationContext<TRequest>(request);
        
        var validationResults = await Task.WhenAll(
            _validators.Select(v => v.ValidateAsync(context, cancellationToken)));
        
        var failures = validationResults
            .SelectMany(r => r.Errors)
            .Where(f => f != null)
            .ToList();
        
        if (failures.Any())
            throw new ValidationException(failures);
        
        return await next();
    }
}
```

### Logging Behavior
**Runs**: Before and after all commands and queries  
**Purpose**: Log request/response for observability

```csharp
public class LoggingBehavior<TRequest, TResponse> 
    : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly ILogger<LoggingBehavior<TRequest, TResponse>> _logger;
    private readonly ICurrentUserService _currentUserService;
    
    public async Task<TResponse> Handle(
        TRequest request,
        RequestHandlerDelegate<TResponse> next,
        CancellationToken cancellationToken)
    {
        var requestName = typeof(TRequest).Name;
        var userId = _currentUserService.UserId ?? "Anonymous";
        
        _logger.LogInformation(
            "Handling {RequestName} by user {UserId} with {@Request}",
            requestName, userId, request);
        
        var stopwatch = Stopwatch.StartNew();
        
        try
        {
            var response = await next();
            
            stopwatch.Stop();
            
            _logger.LogInformation(
                "Handled {RequestName} in {ElapsedMilliseconds}ms",
                requestName, stopwatch.ElapsedMilliseconds);
            
            return response;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            
            _logger.LogError(ex, 
                "Error handling {RequestName} in {ElapsedMilliseconds}ms",
                requestName, stopwatch.ElapsedMilliseconds);
            
            throw;
        }
    }
}
```

### Transaction Behavior
**Runs**: Around commands only (not queries)  
**Purpose**: Wrap commands in database transaction

```csharp
public class TransactionBehavior<TRequest, TResponse> 
    : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly IUnitOfWork _unitOfWork;
    private readonly ILogger<TransactionBehavior<TRequest, TResponse>> _logger;
    
    public async Task<TResponse> Handle(
        TRequest request,
        RequestHandlerDelegate<TResponse> next,
        CancellationToken cancellationToken)
    {
        // Skip transaction for queries
        if (IsQuery(request))
            return await next();
        
        _logger.LogInformation("Beginning transaction for {RequestName}", typeof(TRequest).Name);
        
        await using var transaction = await _unitOfWork.BeginTransactionAsync(cancellationToken);
        
        try
        {
            var response = await next();
            await transaction.CommitAsync(cancellationToken);
            
            _logger.LogInformation("Committed transaction for {RequestName}", typeof(TRequest).Name);
            
            return response;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Rolling back transaction for {RequestName}", typeof(TRequest).Name);
            await transaction.RollbackAsync(cancellationToken);
            throw;
        }
    }
    
    private static bool IsQuery(TRequest request) =>
        request.GetType().Name.EndsWith("Query");
}
```

### Performance Behavior
**Runs**: Around all commands and queries  
**Purpose**: Track slow requests

```csharp
public class PerformanceBehavior<TRequest, TResponse> 
    : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly ILogger<PerformanceBehavior<TRequest, TResponse>> _logger;
    private readonly Stopwatch _timer;
    
    public PerformanceBehavior(ILogger<PerformanceBehavior<TRequest, TResponse>> logger)
    {
        _logger = logger;
        _timer = new Stopwatch();
    }
    
    public async Task<TResponse> Handle(
        TRequest request,
        RequestHandlerDelegate<TResponse> next,
        CancellationToken cancellationToken)
    {
        _timer.Start();
        var response = await next();
        _timer.Stop();
        
        var elapsedMilliseconds = _timer.ElapsedMilliseconds;
        
        if (elapsedMilliseconds > 500) // Threshold for slow requests
        {
            var requestName = typeof(TRequest).Name;
            _logger.LogWarning(
                "Long Running Request: {RequestName} ({ElapsedMilliseconds} ms) {@Request}",
                requestName, elapsedMilliseconds, request);
        }
        
        return response;
    }
}
```

## Controller Integration

Controllers are thin, delegating to MediatR:

```csharp
[ApiController]
[Route("api/[controller]")]
public class AccountsController : ControllerBase
{
    private readonly IMediator _mediator;
    
    public AccountsController(IMediator mediator) => _mediator = mediator;
    
    /// <summary>
    /// Creates a new account
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(Guid), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> CreateAccount(
        [FromBody] CreateAccountRequest request,
        CancellationToken cancellationToken)
    {
        var command = new CreateAccountCommand(
            request.CustomerId,
            request.CreditLimit,
            request.AccountGroupId);
        
        var result = await _mediator.Send(command, cancellationToken);
        
        if (result.IsSuccess)
        {
            return CreatedAtAction(
                nameof(GetAccount),
                new { id = result.Value },
                result.Value);
        }
        
        return BadRequest(new { error = result.Error });
    }
    
    /// <summary>
    /// Gets account by ID
    /// </summary>
    [HttpGet("{id}")]
    [ProducesResponseType(typeof(AccountDto), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
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

## Registration (Dependency Injection)

**In CardDemo.Application/DependencyInjection.cs**:

```csharp
public static class DependencyInjection
{
    public static IServiceCollection AddApplicationServices(this IServiceCollection services)
    {
        // Register MediatR
        services.AddMediatR(cfg => 
            cfg.RegisterServicesFromAssembly(Assembly.GetExecutingAssembly()));
        
        // Register FluentValidation validators
        services.AddValidatorsFromAssembly(Assembly.GetExecutingAssembly());
        
        // Register AutoMapper
        services.AddAutoMapper(Assembly.GetExecutingAssembly());
        
        // Register pipeline behaviors (order matters!)
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(ValidationBehavior<,>));
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(LoggingBehavior<,>));
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(TransactionBehavior<,>));
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(PerformanceBehavior<,>));
        
        return services;
    }
}
```

## Testing

### Unit Test Command Handler

```csharp
public class CreateAccountCommandHandlerTests
{
    private readonly Mock<IAccountRepository> _repositoryMock;
    private readonly Mock<IUnitOfWork> _unitOfWorkMock;
    private readonly Mock<IEventPublisher> _eventPublisherMock;
    private readonly CreateAccountCommandHandler _handler;
    
    public CreateAccountCommandHandlerTests()
    {
        _repositoryMock = new Mock<IAccountRepository>();
        _unitOfWorkMock = new Mock<IUnitOfWork>();
        _eventPublisherMock = new Mock<IEventPublisher>();
        
        _handler = new CreateAccountCommandHandler(
            _repositoryMock.Object,
            _unitOfWorkMock.Object,
            _eventPublisherMock.Object,
            Mock.Of<ILogger<CreateAccountCommandHandler>>());
    }
    
    [Fact]
    public async Task Handle_ValidCommand_ReturnsSuccessResult()
    {
        // Arrange
        var command = new CreateAccountCommand(
            "00000000001",
            5000m,
            "DEFAULT");
        
        // Act
        var result = await _handler.Handle(command, CancellationToken.None);
        
        // Assert
        result.IsSuccess.Should().BeTrue();
        result.Value.Should().NotBeEmpty();
        
        _repositoryMock.Verify(
            x => x.AddAsync(It.IsAny<Account>(), It.IsAny<CancellationToken>()),
            Times.Once);
        
        _unitOfWorkMock.Verify(
            x => x.SaveChangesAsync(It.IsAny<CancellationToken>()),
            Times.Once);
    }
}
```

### Unit Test Validator

```csharp
public class CreateAccountCommandValidatorTests
{
    private readonly CreateAccountCommandValidator _validator;
    
    public CreateAccountCommandValidatorTests()
    {
        _validator = new CreateAccountCommandValidator();
    }
    
    [Fact]
    public void Validate_ValidCommand_ShouldNotHaveValidationError()
    {
        // Arrange
        var command = new CreateAccountCommand("00000000001", 5000m, "DEFAULT");
        
        // Act
        var result = _validator.Validate(command);
        
        // Assert
        result.IsValid.Should().BeTrue();
    }
    
    [Theory]
    [InlineData("", "Customer ID is required")]
    [InlineData("123", "Customer ID must be exactly 11 characters")]
    public void Validate_InvalidCustomerId_ShouldHaveValidationError(
        string customerId, string expectedMessage)
    {
        // Arrange
        var command = new CreateAccountCommand(customerId, 5000m, "DEFAULT");
        
        // Act
        var result = _validator.Validate(command);
        
        // Assert
        result.IsValid.Should().BeFalse();
        result.Errors.Should().Contain(e => e.ErrorMessage == expectedMessage);
    }
}
```

## Best Practices

### 1. Naming Conventions
- **Commands**: `{Verb}{Entity}Command` (e.g., `CreateAccountCommand`, `UpdateCardCommand`)
- **Queries**: `Get{Entity}Query`, `List{Entities}Query` (e.g., `GetAccountQuery`, `ListTransactionsQuery`)
- **Handlers**: `{CommandOrQuery}Handler`
- **Validators**: `{Command}Validator`
- **DTOs**: `{Entity}Dto`, `{Entity}ListItemDto`

### 2. Command Design
- Use `record` types (immutable)
- Include all required data in constructor
- No business logic in commands
- Return `Result<T>` for error handling

### 3. Handler Design
- Single responsibility (one handler per command/query)
- Depend on interfaces (not concrete classes)
- Keep handlers focused (extract complex logic to domain services)
- Log important events and errors

### 4. Query Optimization
- Return DTOs (not domain entities)
- Use projections to select only needed columns
- Apply caching for frequently accessed data
- Consider read models for complex queries

### 5. Validation
- Use FluentValidation for declarative rules
- Validate in pipeline behavior (before handler)
- Provide descriptive error messages
- Keep validators simple (no business logic)

## Common Pitfalls

### ❌ Avoid: Direct Repository Access in Controllers
```csharp
// BAD: Controller has business logic
public async Task<IActionResult> CreateAccount(CreateAccountRequest request)
{
    var account = new Account { ... };
    await _repository.AddAsync(account);
    await _unitOfWork.SaveChangesAsync();
    return Ok();
}
```

### ✅ Do: Use MediatR Commands
```csharp
// GOOD: Controller delegates to command
public async Task<IActionResult> CreateAccount(CreateAccountRequest request)
{
    var command = new CreateAccountCommand(request.CustomerId, request.CreditLimit);
    var result = await _mediator.Send(command);
    return result.IsSuccess ? Ok(result.Value) : BadRequest(result.Error);
}
```

### ❌ Avoid: Returning Domain Entities from Queries
```csharp
// BAD: Exposes domain entity
public record GetAccountQuery(string Id) : IRequest<Account>;
```

### ✅ Do: Return DTOs from Queries
```csharp
// GOOD: Returns DTO
public record GetAccountQuery(string Id) : IRequest<AccountDto>;
```

## Related Patterns

- **Repository Pattern**: Handlers depend on repositories for data access
- **Unit of Work**: Handlers use Unit of Work for transactional consistency
- **Result Pattern**: Commands return `Result<T>` for error handling
- **Domain Events**: Handlers publish domain events for side effects

## References

- [MediatR Documentation](https://github.com/jbogard/MediatR)
- [FluentValidation Documentation](https://docs.fluentvalidation.net/)
- [CQRS Pattern - Martin Fowler](https://martinfowler.com/bliki/CQRS.html)
- [Clean Architecture - Jason Taylor](https://jasontaylor.dev/clean-architecture-getting-started/)
