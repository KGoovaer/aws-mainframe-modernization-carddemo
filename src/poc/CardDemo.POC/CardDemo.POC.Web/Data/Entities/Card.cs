namespace CardDemo.POC.Web.Data.Entities;

/// <summary>
/// Card entity - maps to COBOL CARD file
/// </summary>
public class Card
{
    /// <summary>
    /// Card number - maps to COBOL CARD-NUM PIC 9(16)
    /// </summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>
    /// Account ID - maps to COBOL ACCT-ID PIC 9(11)
    /// </summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Card type - maps to COBOL CARD-TYPE PIC X(10)
    /// </summary>
    public string CardType { get; set; } = string.Empty;

    /// <summary>
    /// Expiration date
    /// </summary>
    public DateTime ExpirationDate { get; set; }

    /// <summary>
    /// Status: A=Active, S=Suspended, C=Closed
    /// </summary>
    public string Status { get; set; } = "A";

    /// <summary>
    /// Navigation property to account
    /// </summary>
    public Account? Account { get; set; }
}
