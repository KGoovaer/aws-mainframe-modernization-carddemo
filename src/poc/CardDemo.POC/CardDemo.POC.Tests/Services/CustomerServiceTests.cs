using CardDemo.POC.Web.Data;
using CardDemo.POC.Web.Data.Entities;
using CardDemo.POC.Web.Services;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using NSubstitute;
using Xunit;

namespace CardDemo.POC.Tests.Services;

public class CustomerServiceTests
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
    public async Task GetAllCustomersAsync_ReturnsAllCustomers()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<CustomerService>>();
        var service = new CustomerService(context, logger);

        context.Customers.AddRange(
            new Customer
            {
                CustomerId = "000000001",
                FirstName = "John",
                LastName = "Doe",
                FicoCreditScore = 750
            },
            new Customer
            {
                CustomerId = "000000002",
                FirstName = "Jane",
                LastName = "Smith",
                FicoCreditScore = 800
            }
        );
        await context.SaveChangesAsync();

        // Act
        var result = await service.GetAllCustomersAsync();

        // Assert
        Assert.NotNull(result);
        Assert.Equal(2, result.Count);
    }

    [Fact]
    public async Task CreateCustomerAsync_ValidCustomer_CreatesCustomer()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<CustomerService>>();
        var service = new CustomerService(context, logger);

        var customer = new Customer
        {
            CustomerId = "000000001",
            FirstName = "John",
            LastName = "Doe",
            FicoCreditScore = 750
        };

        // Act
        var result = await service.CreateCustomerAsync(customer);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(customer.CustomerId, result.CustomerId);

        var savedCustomer = await context.Customers
            .FirstOrDefaultAsync(c => c.CustomerId == customer.CustomerId);
        Assert.NotNull(savedCustomer);
    }

    [Fact]
    public async Task CreateCustomerAsync_DuplicateCustomer_ThrowsException()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<CustomerService>>();
        var service = new CustomerService(context, logger);

        context.Customers.Add(new Customer
        {
            CustomerId = "000000001",
            FirstName = "John",
            LastName = "Doe",
            FicoCreditScore = 750
        });
        await context.SaveChangesAsync();

        var duplicateCustomer = new Customer
        {
            CustomerId = "000000001",
            FirstName = "Jane",
            LastName = "Smith",
            FicoCreditScore = 800
        };

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(
            () => service.CreateCustomerAsync(duplicateCustomer));
    }

    [Fact]
    public async Task GetCustomerAsync_ExistingCustomer_ReturnsCustomer()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<CustomerService>>();
        var service = new CustomerService(context, logger);

        var customer = new Customer
        {
            CustomerId = "000000001",
            FirstName = "John",
            LastName = "Doe",
            FicoCreditScore = 750
        };
        context.Customers.Add(customer);
        await context.SaveChangesAsync();

        // Act
        var result = await service.GetCustomerAsync("000000001");

        // Assert
        Assert.NotNull(result);
        Assert.Equal("John", result.FirstName);
        Assert.Equal("Doe", result.LastName);
    }

    [Fact]
    public async Task GetCustomerAsync_NonExistentCustomer_ReturnsNull()
    {
        // Arrange
        using var context = CreateInMemoryContext();
        var logger = Substitute.For<ILogger<CustomerService>>();
        var service = new CustomerService(context, logger);

        // Act
        var result = await service.GetCustomerAsync("999999999");

        // Assert
        Assert.Null(result);
    }
}
