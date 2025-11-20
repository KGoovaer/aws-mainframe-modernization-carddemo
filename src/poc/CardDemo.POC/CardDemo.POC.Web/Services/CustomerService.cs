using CardDemo.POC.Web.Data;
using CardDemo.POC.Web.Data.Entities;
using Microsoft.EntityFrameworkCore;

namespace CardDemo.POC.Web.Services;

/// <summary>
/// Service for managing customer operations
/// </summary>
public class CustomerService
{
    private readonly CardDemoDbContext _context;
    private readonly ILogger<CustomerService> _logger;

    public CustomerService(CardDemoDbContext context, ILogger<CustomerService> logger)
    {
        _context = context;
        _logger = logger;
    }

    /// <summary>
    /// Get all customers
    /// </summary>
    public async Task<List<Customer>> GetAllCustomersAsync()
    {
        return await _context.Customers
            .Include(c => c.Accounts)
            .ToListAsync();
    }

    /// <summary>
    /// Get customer by ID
    /// </summary>
    public async Task<Customer?> GetCustomerAsync(string customerId)
    {
        return await _context.Customers
            .Include(c => c.Accounts)
            .FirstOrDefaultAsync(c => c.CustomerId == customerId);
    }

    /// <summary>
    /// Create a new customer
    /// </summary>
    public async Task<Customer> CreateCustomerAsync(Customer customer)
    {
        _logger.LogInformation("Creating customer {CustomerId}", customer.CustomerId);

        // Validate customer doesn't already exist
        var exists = await _context.Customers
            .AnyAsync(c => c.CustomerId == customer.CustomerId);
        
        if (exists)
        {
            throw new InvalidOperationException(
                $"Customer {customer.CustomerId} already exists");
        }

        _context.Customers.Add(customer);
        await _context.SaveChangesAsync();

        _logger.LogInformation("Customer {CustomerId} created successfully", customer.CustomerId);

        return customer;
    }

    /// <summary>
    /// Update an existing customer
    /// </summary>
    public async Task UpdateCustomerAsync(Customer customer)
    {
        _logger.LogInformation("Updating customer {CustomerId}", customer.CustomerId);

        _context.Customers.Update(customer);
        await _context.SaveChangesAsync();

        _logger.LogInformation("Customer {CustomerId} updated successfully", customer.CustomerId);
    }

    /// <summary>
    /// Delete a customer
    /// </summary>
    public async Task DeleteCustomerAsync(string customerId)
    {
        _logger.LogInformation("Deleting customer {CustomerId}", customerId);

        var customer = await GetCustomerAsync(customerId);
        if (customer != null)
        {
            _context.Customers.Remove(customer);
            await _context.SaveChangesAsync();

            _logger.LogInformation("Customer {CustomerId} deleted successfully", customerId);
        }
    }
}
