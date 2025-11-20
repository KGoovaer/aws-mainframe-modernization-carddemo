using CardDemo.POC.Web.Data.Entities;
using CardDemo.POC.Web.Services;
using Microsoft.AspNetCore.Mvc;

namespace CardDemo.POC.Web.Controllers;

/// <summary>
/// API controller for account operations
/// </summary>
[ApiController]
[Route("api/[controller]")]
public class AccountsController : ControllerBase
{
    private readonly AccountService _accountService;
    private readonly ILogger<AccountsController> _logger;

    public AccountsController(
        AccountService accountService,
        ILogger<AccountsController> logger)
    {
        _accountService = accountService;
        _logger = logger;
    }

    /// <summary>
    /// Get all accounts
    /// </summary>
    [HttpGet]
    public async Task<ActionResult<List<Account>>> GetAllAccounts()
    {
        var accounts = await _accountService.GetAllAccountsAsync();
        return Ok(accounts);
    }

    /// <summary>
    /// Get account by ID
    /// </summary>
    [HttpGet("{accountId}")]
    public async Task<ActionResult<Account>> GetAccount(string accountId)
    {
        var account = await _accountService.GetAccountAsync(accountId);
        if (account == null)
            return NotFound();

        return Ok(account);
    }

    /// <summary>
    /// Create new account
    /// </summary>
    [HttpPost]
    public async Task<ActionResult<Account>> CreateAccount(Account account)
    {
        try
        {
            var created = await _accountService.CreateAccountAsync(account);
            return CreatedAtAction(nameof(GetAccount), 
                new { accountId = created.AccountId }, 
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
    /// Update existing account
    /// </summary>
    [HttpPut("{accountId}")]
    public async Task<IActionResult> UpdateAccount(string accountId, Account account)
    {
        if (accountId != account.AccountId)
            return BadRequest("Account ID mismatch");

        try
        {
            await _accountService.UpdateAccountAsync(account);
            return NoContent();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error updating account {AccountId}", accountId);
            return StatusCode(500, "An error occurred while updating the account");
        }
    }

    /// <summary>
    /// Delete account
    /// </summary>
    [HttpDelete("{accountId}")]
    public async Task<IActionResult> DeleteAccount(string accountId)
    {
        try
        {
            await _accountService.DeleteAccountAsync(accountId);
            return NoContent();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error deleting account {AccountId}", accountId);
            return StatusCode(500, "An error occurred while deleting the account");
        }
    }
}
