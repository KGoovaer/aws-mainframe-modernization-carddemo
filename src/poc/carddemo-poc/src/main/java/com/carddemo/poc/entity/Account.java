package com.carddemo.poc.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Account entity - maps to COBOL CVACT01Y copybook.
 * Represents a credit card account with balances, limits, and lifecycle information.
 */
@Entity
@Table(name = "accounts")
public class Account {
    
    /**
     * Account ID - maps to COBOL ACCT-ID PIC 9(11)
     */
    @Id
    @Column(name = "account_id", length = 11, nullable = false)
    private String accountId;
    
    /**
     * Customer ID - maps to COBOL CUST-ID (FK to customers table)
     */
    @Column(name = "customer_id", length = 9, nullable = false)
    private String customerId;
    
    /**
     * Active status: Y=Active, N=Inactive
     * Maps to COBOL ACCT-ACTIVE-STATUS PIC X(01)
     */
    @Column(name = "active_status", length = 1, nullable = false)
    private String activeStatus;
    
    /**
     * Current balance - maps to COBOL ACCT-CURR-BAL PIC S9(10)V99
     */
    @Column(name = "current_balance", precision = 12, scale = 2, nullable = false)
    private BigDecimal currentBalance;
    
    /**
     * Credit limit - maps to COBOL ACCT-CREDIT-LIMIT PIC S9(10)V99
     */
    @Column(name = "credit_limit", precision = 12, scale = 2, nullable = false)
    private BigDecimal creditLimit;
    
    /**
     * Cash credit limit - maps to COBOL ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
     */
    @Column(name = "cash_credit_limit", precision = 12, scale = 2, nullable = false)
    private BigDecimal cashCreditLimit;
    
    /**
     * Date account was opened - maps to COBOL ACCT-OPEN-DATE PIC X(10)
     */
    @Column(name = "open_date", nullable = false)
    private LocalDate openDate;
    
    /**
     * Account expiration date - maps to COBOL ACCT-EXPIRAION-DATE PIC X(10)
     */
    @Column(name = "expiration_date")
    private LocalDate expirationDate;
    
    /**
     * Date account was reissued - maps to COBOL ACCT-REISSUE-DATE PIC X(10)
     */
    @Column(name = "reissue_date")
    private LocalDate reissueDate;
    
    /**
     * Current cycle credit total - maps to COBOL ACCT-CURR-CYC-CREDIT PIC S9(10)V99
     */
    @Column(name = "current_cycle_credit", precision = 12, scale = 2, nullable = false)
    private BigDecimal currentCycleCredit;
    
    /**
     * Current cycle debit total - maps to COBOL ACCT-CURR-CYC-DEBIT PIC S9(10)V99
     */
    @Column(name = "current_cycle_debit", precision = 12, scale = 2, nullable = false)
    private BigDecimal currentCycleDebit;
    
    /**
     * Address ZIP code - maps to COBOL ACCT-ADDR-ZIP PIC X(10)
     */
    @Column(name = "address_zip", length = 10)
    private String addressZip;
    
    /**
     * Group ID for rate determination - maps to COBOL ACCT-GROUP-ID PIC X(10)
     */
    @Column(name = "group_id", length = 10)
    private String groupId;
    
    /**
     * Navigation property to customer (lazy loaded for performance)
     */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "customer_id", referencedColumnName = "customer_id", 
                insertable = false, updatable = false)
    private Customer customer;
    
    // Constructors
    public Account() {
    }
    
    // Getters and Setters
    public String getAccountId() {
        return accountId;
    }
    
    public void setAccountId(String accountId) {
        this.accountId = accountId;
    }
    
    public String getCustomerId() {
        return customerId;
    }
    
    public void setCustomerId(String customerId) {
        this.customerId = customerId;
    }
    
    public String getActiveStatus() {
        return activeStatus;
    }
    
    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }
    
    public BigDecimal getCurrentBalance() {
        return currentBalance;
    }
    
    public void setCurrentBalance(BigDecimal currentBalance) {
        this.currentBalance = currentBalance;
    }
    
    public BigDecimal getCreditLimit() {
        return creditLimit;
    }
    
    public void setCreditLimit(BigDecimal creditLimit) {
        this.creditLimit = creditLimit;
    }
    
    public BigDecimal getCashCreditLimit() {
        return cashCreditLimit;
    }
    
    public void setCashCreditLimit(BigDecimal cashCreditLimit) {
        this.cashCreditLimit = cashCreditLimit;
    }
    
    public LocalDate getOpenDate() {
        return openDate;
    }
    
    public void setOpenDate(LocalDate openDate) {
        this.openDate = openDate;
    }
    
    public LocalDate getExpirationDate() {
        return expirationDate;
    }
    
    public void setExpirationDate(LocalDate expirationDate) {
        this.expirationDate = expirationDate;
    }
    
    public LocalDate getReissueDate() {
        return reissueDate;
    }
    
    public void setReissueDate(LocalDate reissueDate) {
        this.reissueDate = reissueDate;
    }
    
    public BigDecimal getCurrentCycleCredit() {
        return currentCycleCredit;
    }
    
    public void setCurrentCycleCredit(BigDecimal currentCycleCredit) {
        this.currentCycleCredit = currentCycleCredit;
    }
    
    public BigDecimal getCurrentCycleDebit() {
        return currentCycleDebit;
    }
    
    public void setCurrentCycleDebit(BigDecimal currentCycleDebit) {
        this.currentCycleDebit = currentCycleDebit;
    }
    
    public String getAddressZip() {
        return addressZip;
    }
    
    public void setAddressZip(String addressZip) {
        this.addressZip = addressZip;
    }
    
    public String getGroupId() {
        return groupId;
    }
    
    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }
    
    public Customer getCustomer() {
        return customer;
    }
    
    public void setCustomer(Customer customer) {
        this.customer = customer;
    }
}
