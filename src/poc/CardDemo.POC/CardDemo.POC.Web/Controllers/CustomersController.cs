using CardDemo.POC.Web.Data.Entities;
using CardDemo.POC.Web.Services;
using Microsoft.AspNetCore.Mvc;

namespace CardDemo.POC.Web.Controllers;

/// <summary>
/// API controller for customer operations
/// </summary>
[ApiController]
[Route("api/[controller]")]
public class CustomersController : ControllerBase
{
    private readonly CustomerService _customerService;
    private readonly ILogger<CustomersController> _logger;

    public CustomersController(
        CustomerService customerService,
        ILogger<CustomersController> logger)
    {
        _customerService = customerService;
        _logger = logger;
    }

    /// <summary>
    /// Get all customers
    /// </summary>
    [HttpGet]
    public async Task<ActionResult<List<Customer>>> GetAllCustomers()
    {
        var customers = await _customerService.GetAllCustomersAsync();
        return Ok(customers);
    }

    /// <summary>
    /// Get customer by ID
    /// </summary>
    [HttpGet("{customerId}")]
    public async Task<ActionResult<Customer>> GetCustomer(string customerId)
    {
        var customer = await _customerService.GetCustomerAsync(customerId);
        if (customer == null)
            return NotFound();

        return Ok(customer);
    }

    /// <summary>
    /// Create new customer
    /// </summary>
    [HttpPost]
    public async Task<ActionResult<Customer>> CreateCustomer(Customer customer)
    {
        try
        {
            var created = await _customerService.CreateCustomerAsync(customer);
            return CreatedAtAction(nameof(GetCustomer), 
                new { customerId = created.CustomerId }, 
                created);
        }
        catch (InvalidOperationException ex)
        {
            return Conflict(ex.Message);
        }
        catch (ArgumentException ex)
        {
            return BadRequest(ex.Message);
        }
    }

    /// <summary>
    /// Update existing customer
    /// </summary>
    [HttpPut("{customerId}")]
    public async Task<IActionResult> UpdateCustomer(string customerId, Customer customer)
    {
        if (customerId != customer.CustomerId)
            return BadRequest("Customer ID mismatch");

        try
        {
            await _customerService.UpdateCustomerAsync(customer);
            return NoContent();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error updating customer {CustomerId}", customerId);
            return StatusCode(500, "An error occurred while updating the customer");
        }
    }

    /// <summary>
    /// Delete customer
    /// </summary>
    [HttpDelete("{customerId}")]
    public async Task<IActionResult> DeleteCustomer(string customerId)
    {
        try
        {
            await _customerService.DeleteCustomerAsync(customerId);
            return NoContent();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error deleting customer {CustomerId}", customerId);
            return StatusCode(500, "An error occurred while deleting the customer");
        }
    }
}
