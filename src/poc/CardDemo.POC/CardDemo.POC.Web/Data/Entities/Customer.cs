namespace CardDemo.POC.Web.Data.Entities;

/// <summary>
/// Customer entity - maps to COBOL CUSTOMER file
/// </summary>
public class Customer
{
    /// <summary>
    /// Customer ID - maps to COBOL CUST-ID PIC 9(9)
    /// </summary>
    public string CustomerId { get; set; } = string.Empty;

    /// <summary>
    /// First name - maps to COBOL CUST-FIRST-NAME PIC X(25)
    /// </summary>
    public string FirstName { get; set; } = string.Empty;

    /// <summary>
    /// Middle name - maps to COBOL CUST-MIDDLE-NAME PIC X(25)
    /// </summary>
    public string MiddleName { get; set; } = string.Empty;

    /// <summary>
    /// Last name - maps to COBOL CUST-LAST-NAME PIC X(25)
    /// </summary>
    public string LastName { get; set; } = string.Empty;

    /// <summary>
    /// FICO credit score - maps to COBOL CUST-FICO-CREDIT-SCORE PIC 9(3)
    /// </summary>
    public int FicoCreditScore { get; set; }

    /// <summary>
    /// Phone number
    /// </summary>
    public string PhoneNumber { get; set; } = string.Empty;

    /// <summary>
    /// Navigation property to accounts
    /// </summary>
    public List<Account> Accounts { get; set; } = new();
}
