package com.carddemo.poc.service;

import com.carddemo.poc.dto.AccountDetailsDto;
import com.carddemo.poc.dto.CustomerDto;
import com.carddemo.poc.dto.UpdateAccountAndCustomerRequest;
import com.carddemo.poc.entity.Account;
import com.carddemo.poc.entity.Customer;
import com.carddemo.poc.repository.AccountRepository;
import com.carddemo.poc.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Service for account operations with business rules validation.
 * Implements FR-002.1 (Account Inquiry) and FR-002.2 (Account/Customer Update).
 */
@Service
@Transactional
public class AccountService {
    
    private static final Logger log = LoggerFactory.getLogger(AccountService.class);
    
    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    
    public AccountService(AccountRepository accountRepository, 
                         CustomerRepository customerRepository) {
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
    }
    
    /**
     * Get account details with customer information (FR-002.1).
     * 
     * @param accountId Account ID to retrieve
     * @return Account details with customer information
     * @throws IllegalArgumentException if account not found
     */
    @Transactional(readOnly = true)
    public AccountDetailsDto getAccountDetails(String accountId) {
        log.info("Retrieving account details for account {}", accountId);
        
        // Validate account ID format (Rule 002-1: 11 digits)
        if (accountId == null || !accountId.matches("\\d{11}")) {
            throw new IllegalArgumentException("Account ID must be exactly 11 digits");
        }
        
        // Fetch account with customer (optimized query)
        Account account = accountRepository.findByIdWithCustomer(accountId)
            .orElseThrow(() -> new IllegalArgumentException(
                "Account not found: " + accountId));
        
        // Fetch customer if not loaded
        Customer customer = account.getCustomer();
        if (customer == null) {
            customer = customerRepository.findById(account.getCustomerId())
                .orElseThrow(() -> new IllegalStateException(
                    "Customer not found for account: " + accountId));
        }
        
        log.debug("Account found: {}, customer: {}", 
                 accountId, customer.getCustomerId());
        
        // Build response DTO
        return new AccountDetailsDto(
            account.getAccountId(),
            account.getCustomerId(),
            account.getActiveStatus(),
            account.getCurrentBalance(),
            account.getCreditLimit(),
            account.getCashCreditLimit(),
            account.getOpenDate(),
            account.getExpirationDate(),
            account.getReissueDate(),
            account.getCurrentCycleCredit(),
            account.getCurrentCycleDebit(),
            account.getAddressZip(),
            account.getGroupId(),
            mapCustomerToDto(customer)
        );
    }
    
    /**
     * Update account and customer information transactionally (FR-002.2).
     * Enforces business rules: credit limit validation, date validation, 
     * FICO score range, and transactional integrity.
     * 
     * @param accountId Account ID to update
     * @param request Update request with account and customer changes
     * @throws IllegalArgumentException if validation fails
     * @throws IllegalStateException if account or customer not found
     */
    public void updateAccountAndCustomer(String accountId, 
                                        UpdateAccountAndCustomerRequest request) {
        log.info("Updating account {} and customer", accountId);
        
        // Validate account ID format (Rule 002-1)
        if (accountId == null || !accountId.matches("\\d{11}")) {
            throw new IllegalArgumentException("Account ID must be exactly 11 digits");
        }
        
        // Fetch account
        Account account = accountRepository.findById(accountId)
            .orElseThrow(() -> new IllegalStateException(
                "Account not found: " + accountId));
        
        // Fetch customer
        Customer customer = customerRepository.findById(account.getCustomerId())
            .orElseThrow(() -> new IllegalStateException(
                "Customer not found: " + account.getCustomerId()));
        
        // Validate ALL changes BEFORE applying any updates (Rule 002-4: transactional integrity)
        if (request.account() != null) {
            validateAccountUpdates(account, request.account());
        }
        if (request.customer() != null) {
            validateCustomerUpdates(customer, request.customer());
        }
        
        // Apply updates only after all validations pass
        if (request.account() != null) {
            applyAccountUpdates(account, request.account());
        }
        
        if (request.customer() != null) {
            applyCustomerUpdates(customer, request.customer());
        }
        
        // Save both (transactional - Rule 002-4: both succeed or both fail)
        accountRepository.save(account);
        customerRepository.save(customer);
        
        log.info("Account {} and customer {} updated successfully", 
                accountId, customer.getCustomerId());
    }
    
    /**
     * Validate account updates before applying (Rule 002-2, 002-3).
     */
    private void validateAccountUpdates(Account account, 
                                        UpdateAccountAndCustomerRequest.AccountUpdate update) {
        if (update.creditLimit() != null) {
            validateCreditLimit(account.getCurrentBalance(), update.creditLimit());
        }
        
        if (update.expirationDate() != null) {
            validateExpirationDate(account.getOpenDate(), update.expirationDate());
        }
        
        if (update.reissueDate() != null) {
            validateReissueDate(account.getOpenDate(), update.reissueDate());
        }
        
        if (update.cashCreditLimit() != null && 
            update.cashCreditLimit().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Cash credit limit cannot be negative");
        }
        
        if (update.activeStatus() != null && !update.activeStatus().matches("[YN]")) {
            throw new IllegalArgumentException("Active status must be Y or N");
        }
    }
    
    /**
     * Validate customer updates before applying (Rule 002-7).
     */
    private void validateCustomerUpdates(Customer customer, 
                                         UpdateAccountAndCustomerRequest.CustomerUpdate update) {
        if (update.ficoScore() != null) {
            validateFicoScore(update.ficoScore());
        }
    }
    
    /**
     * Apply validated account updates (no further validation needed).
     */
    private void applyAccountUpdates(Account account, 
                                     UpdateAccountAndCustomerRequest.AccountUpdate update) {
        if (update.creditLimit() != null) {
            account.setCreditLimit(update.creditLimit());
        }
        
        if (update.cashCreditLimit() != null) {
            account.setCashCreditLimit(update.cashCreditLimit());
        }
        
        if (update.activeStatus() != null) {
            account.setActiveStatus(update.activeStatus());
        }
        
        if (update.expirationDate() != null) {
            account.setExpirationDate(update.expirationDate());
        }
        
        if (update.reissueDate() != null) {
            account.setReissueDate(update.reissueDate());
        }
        
        if (update.groupId() != null) {
            account.setGroupId(update.groupId());
        }
    }
    
    /**
     * Apply validated customer updates (no further validation needed).
     */
    private void applyCustomerUpdates(Customer customer, 
                                      UpdateAccountAndCustomerRequest.CustomerUpdate update) {
        if (update.addressLine1() != null) {
            customer.setAddressLine1(update.addressLine1());
        }
        if (update.addressLine2() != null) {
            customer.setAddressLine2(update.addressLine2());
        }
        if (update.addressLine3() != null) {
            customer.setAddressLine3(update.addressLine3());
        }
        if (update.stateCode() != null) {
            customer.setStateCode(update.stateCode());
        }
        if (update.countryCode() != null) {
            customer.setCountryCode(update.countryCode());
        }
        if (update.zip() != null) {
            customer.setZip(update.zip());
        }
        
        if (update.phoneNumber1() != null) {
            customer.setPhoneNumber1(update.phoneNumber1());
        }
        if (update.phoneNumber2() != null) {
            customer.setPhoneNumber2(update.phoneNumber2());
        }
        
        if (update.ficoScore() != null) {
            customer.setFicoScore(update.ficoScore());
        }
    }
    
    /**
     * Validate credit limit is not less than current balance (Rule 002-2).
     */
    private void validateCreditLimit(BigDecimal currentBalance, BigDecimal newCreditLimit) {
        if (newCreditLimit.compareTo(currentBalance) < 0) {
            throw new IllegalArgumentException(
                "Credit limit (" + newCreditLimit + 
                ") cannot be less than current balance (" + currentBalance + ")");
        }
        
        // Validate maximum credit limit
        if (newCreditLimit.compareTo(new BigDecimal("9999999999.99")) > 0) {
            throw new IllegalArgumentException(
                "Credit limit exceeds maximum allowed");
        }
    }
    
    /**
     * Validate expiration date is after open date (Rule 002-3).
     */
    private void validateExpirationDate(LocalDate openDate, LocalDate expirationDate) {
        if (expirationDate.isBefore(openDate)) {
            throw new IllegalArgumentException(
                "Expiration date (" + expirationDate + 
                ") must be after open date (" + openDate + ")");
        }
    }
    
    /**
     * Validate reissue date is after or equal to open date (Rule 002-3).
     */
    private void validateReissueDate(LocalDate openDate, LocalDate reissueDate) {
        if (reissueDate.isBefore(openDate)) {
            throw new IllegalArgumentException(
                "Reissue date (" + reissueDate + 
                ") must be on or after open date (" + openDate + ")");
        }
    }
    
    /**
     * Validate FICO score is within valid range (Rule 002-7: 300-850).
     */
    private void validateFicoScore(Integer ficoScore) {
        if (ficoScore < 300 || ficoScore > 850) {
            throw new IllegalArgumentException(
                "FICO score must be between 300 and 850, got: " + ficoScore);
        }
    }
    
    /**
     * Map Customer entity to DTO.
     */
    private CustomerDto mapCustomerToDto(Customer customer) {
        return new CustomerDto(
            customer.getCustomerId(),
            customer.getFirstName(),
            customer.getMiddleName(),
            customer.getLastName(),
            customer.getAddressLine1(),
            customer.getAddressLine2(),
            customer.getAddressLine3(),
            customer.getStateCode(),
            customer.getCountryCode(),
            customer.getZip(),
            customer.getPhoneNumber1(),
            customer.getPhoneNumber2(),
            customer.getSsn(),
            customer.getGovernmentId(),
            customer.getDateOfBirth(),
            customer.getEftAccountId(),
            customer.getPrimaryCardHolder(),
            customer.getFicoScore()
        );
    }
}
