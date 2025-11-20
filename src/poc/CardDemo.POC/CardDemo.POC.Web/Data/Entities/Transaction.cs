namespace CardDemo.POC.Web.Data.Entities;

/// <summary>
/// Transaction entity - maps to COBOL TRANSACTION file
/// </summary>
public class Transaction
{
    /// <summary>
    /// Transaction ID - maps to COBOL TRAN-ID PIC X(16)
    /// </summary>
    public string TransactionId { get; set; } = string.Empty;

    /// <summary>
    /// Account ID - maps to COBOL ACCT-ID PIC 9(11)
    /// </summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Card number - maps to COBOL CARD-NUM PIC 9(16)
    /// </summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>
    /// Transaction type code - maps to COBOL TRAN-TYPE-CD PIC X(2)
    /// </summary>
    public string TransactionTypeCode { get; set; } = string.Empty;

    /// <summary>
    /// Transaction category code - maps to COBOL TRAN-CAT-CD PIC 9(4)
    /// </summary>
    public string TransactionCategoryCode { get; set; } = string.Empty;

    /// <summary>
    /// Transaction amount - maps to COBOL TRAN-AMT PIC S9(9)V99
    /// </summary>
    public decimal Amount { get; set; }

    /// <summary>
    /// Description - maps to COBOL TRAN-DESC PIC X(100)
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Transaction timestamp
    /// </summary>
    public DateTime TransactionDate { get; set; }

    /// <summary>
    /// Navigation property to account
    /// </summary>
    public Account? Account { get; set; }
}
