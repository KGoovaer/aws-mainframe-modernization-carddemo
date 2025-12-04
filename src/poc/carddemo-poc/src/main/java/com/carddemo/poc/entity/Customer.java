package com.carddemo.poc.entity;

import jakarta.persistence.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Customer entity - maps to COBOL CVCUS01Y copybook.
 * Represents an individual customer with personal and contact information.
 */
@Entity
@Table(name = "customers")
public class Customer {
    
    /**
     * Customer ID - maps to COBOL CUST-ID PIC 9(09)
     */
    @Id
    @Column(name = "customer_id", length = 9, nullable = false)
    private String customerId;
    
    /**
     * First name - maps to COBOL CUST-FIRST-NAME PIC X(25)
     */
    @Column(name = "first_name", length = 25, nullable = false)
    private String firstName;
    
    /**
     * Middle name - maps to COBOL CUST-MIDDLE-NAME PIC X(25)
     */
    @Column(name = "middle_name", length = 25)
    private String middleName;
    
    /**
     * Last name - maps to COBOL CUST-LAST-NAME PIC X(25)
     */
    @Column(name = "last_name", length = 25, nullable = false)
    private String lastName;
    
    /**
     * Address line 1 - maps to COBOL CUST-ADDR-LINE-1 PIC X(50)
     */
    @Column(name = "address_line_1", length = 50)
    private String addressLine1;
    
    /**
     * Address line 2 - maps to COBOL CUST-ADDR-LINE-2 PIC X(50)
     */
    @Column(name = "address_line_2", length = 50)
    private String addressLine2;
    
    /**
     * Address line 3 - maps to COBOL CUST-ADDR-LINE-3 PIC X(50)
     */
    @Column(name = "address_line_3", length = 50)
    private String addressLine3;
    
    /**
     * State code - maps to COBOL CUST-ADDR-STATE-CD PIC X(02)
     */
    @Column(name = "state_code", length = 2)
    private String stateCode;
    
    /**
     * Country code - maps to COBOL CUST-ADDR-COUNTRY-CD PIC X(03)
     */
    @Column(name = "country_code", length = 3)
    private String countryCode;
    
    /**
     * ZIP code - maps to COBOL CUST-ADDR-ZIP PIC X(10)
     */
    @Column(name = "zip", length = 10)
    private String zip;
    
    /**
     * Phone number 1 - maps to COBOL CUST-PHONE-NUM-1 PIC X(15)
     */
    @Column(name = "phone_number_1", length = 15)
    private String phoneNumber1;
    
    /**
     * Phone number 2 - maps to COBOL CUST-PHONE-NUM-2 PIC X(15)
     */
    @Column(name = "phone_number_2", length = 15)
    private String phoneNumber2;
    
    /**
     * Social Security Number - maps to COBOL CUST-SSN PIC 9(09)
     */
    @Column(name = "ssn", length = 9)
    private String ssn;
    
    /**
     * Government issued ID - maps to COBOL CUST-GOVT-ISSUED-ID PIC X(20)
     */
    @Column(name = "government_id", length = 20)
    private String governmentId;
    
    /**
     * Date of birth - maps to COBOL CUST-DOB-YYYY-MM-DD PIC X(10)
     */
    @Column(name = "date_of_birth")
    private LocalDate dateOfBirth;
    
    /**
     * EFT account ID - maps to COBOL CUST-EFT-ACCOUNT-ID PIC X(10)
     */
    @Column(name = "eft_account_id", length = 10)
    private String eftAccountId;
    
    /**
     * Primary card holder indicator: Y=Yes, N=No
     * Maps to COBOL CUST-PRI-CARD-HOLDER-IND PIC X(01)
     */
    @Column(name = "primary_card_holder", length = 1)
    private String primaryCardHolder;
    
    /**
     * FICO credit score (300-850) - maps to COBOL CUST-FICO-CREDIT-SCORE PIC 9(03)
     */
    @Column(name = "fico_score")
    private Integer ficoScore;
    
    /**
     * Navigation property to accounts (lazy loaded)
     */
    @OneToMany(mappedBy = "customer", fetch = FetchType.LAZY)
    private List<Account> accounts = new ArrayList<>();
    
    // Constructors
    public Customer() {
    }
    
    // Getters and Setters
    public String getCustomerId() {
        return customerId;
    }
    
    public void setCustomerId(String customerId) {
        this.customerId = customerId;
    }
    
    public String getFirstName() {
        return firstName;
    }
    
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }
    
    public String getMiddleName() {
        return middleName;
    }
    
    public void setMiddleName(String middleName) {
        this.middleName = middleName;
    }
    
    public String getLastName() {
        return lastName;
    }
    
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
    
    public String getAddressLine1() {
        return addressLine1;
    }
    
    public void setAddressLine1(String addressLine1) {
        this.addressLine1 = addressLine1;
    }
    
    public String getAddressLine2() {
        return addressLine2;
    }
    
    public void setAddressLine2(String addressLine2) {
        this.addressLine2 = addressLine2;
    }
    
    public String getAddressLine3() {
        return addressLine3;
    }
    
    public void setAddressLine3(String addressLine3) {
        this.addressLine3 = addressLine3;
    }
    
    public String getStateCode() {
        return stateCode;
    }
    
    public void setStateCode(String stateCode) {
        this.stateCode = stateCode;
    }
    
    public String getCountryCode() {
        return countryCode;
    }
    
    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }
    
    public String getZip() {
        return zip;
    }
    
    public void setZip(String zip) {
        this.zip = zip;
    }
    
    public String getPhoneNumber1() {
        return phoneNumber1;
    }
    
    public void setPhoneNumber1(String phoneNumber1) {
        this.phoneNumber1 = phoneNumber1;
    }
    
    public String getPhoneNumber2() {
        return phoneNumber2;
    }
    
    public void setPhoneNumber2(String phoneNumber2) {
        this.phoneNumber2 = phoneNumber2;
    }
    
    public String getSsn() {
        return ssn;
    }
    
    public void setSsn(String ssn) {
        this.ssn = ssn;
    }
    
    public String getGovernmentId() {
        return governmentId;
    }
    
    public void setGovernmentId(String governmentId) {
        this.governmentId = governmentId;
    }
    
    public LocalDate getDateOfBirth() {
        return dateOfBirth;
    }
    
    public void setDateOfBirth(LocalDate dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }
    
    public String getEftAccountId() {
        return eftAccountId;
    }
    
    public void setEftAccountId(String eftAccountId) {
        this.eftAccountId = eftAccountId;
    }
    
    public String getPrimaryCardHolder() {
        return primaryCardHolder;
    }
    
    public void setPrimaryCardHolder(String primaryCardHolder) {
        this.primaryCardHolder = primaryCardHolder;
    }
    
    public Integer getFicoScore() {
        return ficoScore;
    }
    
    public void setFicoScore(Integer ficoScore) {
        this.ficoScore = ficoScore;
    }
    
    public List<Account> getAccounts() {
        return accounts;
    }
    
    public void setAccounts(List<Account> accounts) {
        this.accounts = accounts;
    }
}
