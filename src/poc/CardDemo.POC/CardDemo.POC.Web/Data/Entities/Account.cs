namespace CardDemo.POC.Web.Data.Entities;

/// <summary>
/// Account entity - maps to COBOL ACCOUNT file
/// </summary>
public class Account
{
    /// <summary>
    /// Account ID - maps to COBOL ACCT-ID PIC 9(11)
    /// </summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Customer ID - maps to COBOL CUST-ID PIC 9(9)
    /// </summary>
    public string CustomerId { get; set; } = string.Empty;

    /// <summary>
    /// Credit limit - maps to COBOL ACCT-CREDIT-LIMIT PIC S9(9)V99
    /// </summary>
    public decimal CreditLimit { get; set; }

    /// <summary>
    /// Current balance - maps to COBOL ACCT-CURR-BAL PIC S9(9)V99
    /// </summary>
    public decimal CurrentBalance { get; set; }

    /// <summary>
    /// Status: A=Active, C=Closed, S=Suspended
    /// Maps to COBOL ACCT-STATUS PIC X
    /// </summary>
    public string Status { get; set; } = "A";

    /// <summary>
    /// Date account was opened
    /// </summary>
    public DateTime OpenDate { get; set; }

    /// <summary>
    /// Navigation property to customer
    /// </summary>
    public Customer? Customer { get; set; }

    /// <summary>
    /// Navigation property to cards
    /// </summary>
    public List<Card> Cards { get; set; } = new();

    /// <summary>
    /// Navigation property to transactions
    /// </summary>
    public List<Transaction> Transactions { get; set; } = new();
}
