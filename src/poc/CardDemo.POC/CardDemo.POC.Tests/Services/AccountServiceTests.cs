using CardDemo.POC.Web.Data;
using CardDemo.POC.Web.Data.Entities;
using CardDemo.POC.Web.Services;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using NSubstitute;
using Xunit;

namespace CardDemo.POC.Tests.Services;

public class AccountServiceTests
{
    private CardDemoDbContext CreateInMemoryContext()
    {
        var options = new DbContextOptionsBuilder<CardDemoDbContext>()
            .UseSqlite("DataSource=:memory:")
            .Options;

        var context = new CardDemoDbContext(options);
        context.Database.OpenConnection();
        context.Database.EnsureCreated();
        return context;
    }

    [Fact]
    public async Task CreateAccountAsync_ValidAccount_CreatesAccount()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<AccountService>>();
        var service = new AccountService(context, logger);

        // Add a customer first
        context.Customers.Add(new Customer
        {
            CustomerId = "000000001",
            FirstName = "John",
            LastName = "Doe",
            FicoCreditScore = 750
        });
        await context.SaveChangesAsync();

        var account = new Account
        {
            AccountId = "00000000001",
            CustomerId = "000000001",
            CreditLimit = 5000.00m,
            CurrentBalance = 0m,
            Status = "A",
            OpenDate = DateTime.UtcNow
        };

        // Act
        var result = await service.CreateAccountAsync(account);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(account.AccountId, result.AccountId);
        Assert.Equal(account.CreditLimit, result.CreditLimit);

        var savedAccount = await context.Accounts
            .FirstOrDefaultAsync(a => a.AccountId == account.AccountId);
        Assert.NotNull(savedAccount);
    }

    [Fact]
    public async Task CreateAccountAsync_DuplicateAccount_ThrowsException()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<AccountService>>();
        var service = new AccountService(context, logger);

        // Add customer and account
        context.Customers.Add(new Customer
        {
            CustomerId = "000000001",
            FirstName = "John",
            LastName = "Doe",
            FicoCreditScore = 750
        });
        context.Accounts.Add(new Account
        {
            AccountId = "00000000001",
            CustomerId = "000000001",
            CreditLimit = 5000.00m,
            Status = "A",
            OpenDate = DateTime.UtcNow
        });
        await context.SaveChangesAsync();

        var duplicateAccount = new Account
        {
            AccountId = "00000000001",
            CustomerId = "000000001",
            CreditLimit = 10000.00m,
            Status = "A",
            OpenDate = DateTime.UtcNow
        };

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(
            () => service.CreateAccountAsync(duplicateAccount));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-100)]
    [InlineData(1000000000)]
    public async Task CreateAccountAsync_InvalidCreditLimit_ThrowsException(decimal creditLimit)
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<AccountService>>();
        var service = new AccountService(context, logger);

        var account = new Account
        {
            AccountId = "00000000001",
            CustomerId = "000000001",
            CreditLimit = creditLimit,
            Status = "A",
            OpenDate = DateTime.UtcNow
        };

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(
            () => service.CreateAccountAsync(account));
    }

    [Fact]
    public async Task GetAccountAsync_ExistingAccount_ReturnsAccount()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<AccountService>>();
        var service = new AccountService(context, logger);

        context.Customers.Add(new Customer
        {
            CustomerId = "000000001",
            FirstName = "John",
            LastName = "Doe",
            FicoCreditScore = 750
        });
        context.Accounts.Add(new Account
        {
            AccountId = "00000000001",
            CustomerId = "000000001",
            CreditLimit = 5000.00m,
            Status = "A",
            OpenDate = DateTime.UtcNow
        });
        await context.SaveChangesAsync();

        // Act
        var result = await service.GetAccountAsync("00000000001");

        // Assert
        Assert.NotNull(result);
        Assert.Equal("00000000001", result.AccountId);
        Assert.Equal(5000.00m, result.CreditLimit);
    }
}
