using CardDemo.POC.Web.Data;
using CardDemo.POC.Web.Data.Entities;
using Microsoft.EntityFrameworkCore;

namespace CardDemo.POC.Web.Services;

/// <summary>
/// Service for managing account operations
/// </summary>
public class AccountService
{
    private readonly CardDemoDbContext _context;
    private readonly ILogger<AccountService> _logger;

    public AccountService(CardDemoDbContext context, ILogger<AccountService> logger)
    {
        _context = context;
        _logger = logger;
    }

    /// <summary>
    /// Get all accounts
    /// </summary>
    public async Task<List<Account>> GetAllAccountsAsync()
    {
        return await _context.Accounts
            .Include(a => a.Customer)
            .Include(a => a.Cards)
            .ToListAsync();
    }

    /// <summary>
    /// Get account by ID
    /// </summary>
    public async Task<Account?> GetAccountAsync(string accountId)
    {
        return await _context.Accounts
            .Include(a => a.Customer)
            .Include(a => a.Cards)
            .Include(a => a.Transactions)
            .FirstOrDefaultAsync(a => a.AccountId == accountId);
    }

    /// <summary>
    /// Create a new account
    /// </summary>
    public async Task<Account> CreateAccountAsync(Account account)
    {
        _logger.LogInformation("Creating account {AccountId}", account.AccountId);

        // Validate account doesn't already exist
        var exists = await _context.Accounts
            .AnyAsync(a => a.AccountId == account.AccountId);
        
        if (exists)
        {
            throw new InvalidOperationException(
                $"Account {account.AccountId} already exists");
        }

        // Validate credit limit
        if (account.CreditLimit <= 0 || account.CreditLimit > 999999999.99m)
        {
            throw new ArgumentException(
                "Credit limit must be between 0 and 999,999,999.99");
        }

        _context.Accounts.Add(account);
        await _context.SaveChangesAsync();

        _logger.LogInformation("Account {AccountId} created successfully", account.AccountId);

        return account;
    }

    /// <summary>
    /// Update an existing account
    /// </summary>
    public async Task UpdateAccountAsync(Account account)
    {
        _logger.LogInformation("Updating account {AccountId}", account.AccountId);

        _context.Accounts.Update(account);
        await _context.SaveChangesAsync();

        _logger.LogInformation("Account {AccountId} updated successfully", account.AccountId);
    }

    /// <summary>
    /// Delete an account
    /// </summary>
    public async Task DeleteAccountAsync(string accountId)
    {
        _logger.LogInformation("Deleting account {AccountId}", accountId);

        var account = await GetAccountAsync(accountId);
        if (account != null)
        {
            _context.Accounts.Remove(account);
            await _context.SaveChangesAsync();

            _logger.LogInformation("Account {AccountId} deleted successfully", accountId);
        }
    }
}
