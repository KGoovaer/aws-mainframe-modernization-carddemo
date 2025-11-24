package com.carddemo.poc.service;

import com.carddemo.poc.dto.AccountDetailsDto;
import com.carddemo.poc.dto.UpdateAccountAndCustomerRequest;
import com.carddemo.poc.entity.Account;
import com.carddemo.poc.entity.Customer;
import com.carddemo.poc.repository.AccountRepository;
import com.carddemo.poc.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for AccountService using in-memory H2 database.
 * Tests business logic and validation rules for account management.
 */
@DataJpaTest
@Import(AccountService.class)
class AccountServiceTest {
    
    @Autowired
    private AccountService accountService;
    
    @Autowired
    private AccountRepository accountRepository;
    
    @Autowired
    private CustomerRepository customerRepository;
    
    @BeforeEach
    void setUp() {
        accountRepository.deleteAll();
        customerRepository.deleteAll();
    }
    
    // ========== FR-002.1: Account Inquiry Tests ==========
    
    @Test
    void getAccountDetails_existingAccount_returnsAccountWithCustomer() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        accountRepository.save(account);
        
        // Act
        AccountDetailsDto result = accountService.getAccountDetails("00000000001");
        
        // Assert
        assertThat(result).isNotNull();
        assertThat(result.accountId()).isEqualTo("00000000001");
        assertThat(result.customerId()).isEqualTo("000000001");
        assertThat(result.creditLimit()).isEqualByComparingTo(new BigDecimal("5000.00"));
        assertThat(result.currentBalance()).isEqualByComparingTo(new BigDecimal("1250.50"));
        
        // Verify customer details are included
        assertThat(result.customer()).isNotNull();
        assertThat(result.customer().customerId()).isEqualTo("000000001");
        assertThat(result.customer().firstName()).isEqualTo("John");
        assertThat(result.customer().lastName()).isEqualTo("Doe");
        assertThat(result.customer().ficoScore()).isEqualTo(720);
    }
    
    @Test
    void getAccountDetails_nonExistentAccount_throwsException() {
        // Act & Assert
        assertThatThrownBy(() -> accountService.getAccountDetails("99999999999"))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Account not found");
    }
    
    @ParameterizedTest
    @ValueSource(strings = {"123", "ABCD", "00000000001A", ""})
    void getAccountDetails_invalidAccountIdFormat_throwsException(String invalidId) {
        // Act & Assert
        assertThatThrownBy(() -> accountService.getAccountDetails(invalidId))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Account ID must be exactly 11 digits");
    }
    
    // ========== FR-002.2: Account/Customer Update Tests ==========
    
    @Test
    void updateAccountAndCustomer_validData_updatesSuccessfully() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                new BigDecimal("7500.00"),  // New credit limit
                new BigDecimal("1500.00"),  // New cash credit limit
                "Y",
                LocalDate.of(2028, 1, 15),
                null,
                "PREMIUM"
            ),
            new UpdateAccountAndCustomerRequest.CustomerUpdate(
                "456 Oak Ave",              // New address
                "Apt 2B",
                null,
                "CA",
                "USA",
                "90210",
                "555-5678",                 // New phone
                null,
                750                         // New FICO score
            )
        );
        
        // Act
        accountService.updateAccountAndCustomer("00000000001", request);
        
        // Assert - verify account updates
        Account updatedAccount = accountRepository.findById("00000000001").orElseThrow();
        assertThat(updatedAccount.getCreditLimit()).isEqualByComparingTo(new BigDecimal("7500.00"));
        assertThat(updatedAccount.getCashCreditLimit()).isEqualByComparingTo(new BigDecimal("1500.00"));
        assertThat(updatedAccount.getExpirationDate()).isEqualTo(LocalDate.of(2028, 1, 15));
        assertThat(updatedAccount.getGroupId()).isEqualTo("PREMIUM");
        
        // Assert - verify customer updates
        Customer updatedCustomer = customerRepository.findById("000000001").orElseThrow();
        assertThat(updatedCustomer.getAddressLine1()).isEqualTo("456 Oak Ave");
        assertThat(updatedCustomer.getAddressLine2()).isEqualTo("Apt 2B");
        assertThat(updatedCustomer.getStateCode()).isEqualTo("CA");
        assertThat(updatedCustomer.getZip()).isEqualTo("90210");
        assertThat(updatedCustomer.getPhoneNumber1()).isEqualTo("555-5678");
        assertThat(updatedCustomer.getFicoScore()).isEqualTo(750);
    }
    
    @Test
    void updateAccountAndCustomer_nonExistentAccount_throwsException() {
        // Arrange
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                new BigDecimal("5000.00"), null, null, null, null, null),
            null
        );
        
        // Act & Assert
        assertThatThrownBy(() -> 
            accountService.updateAccountAndCustomer("99999999999", request))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("Account not found");
    }
    
    // ========== Rule 002-2: Credit Limit Validation Tests ==========
    
    @Test
    void updateAccountAndCustomer_creditLimitBelowBalance_throwsException() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        account.setCurrentBalance(new BigDecimal("2000.00"));
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                new BigDecimal("1500.00"),  // Less than balance
                null, null, null, null, null),
            null
        );
        
        // Act & Assert
        assertThatThrownBy(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Credit limit")
            .hasMessageContaining("cannot be less than current balance");
    }
    
    @Test
    void updateAccountAndCustomer_creditLimitEqualToBalance_succeeds() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        account.setCurrentBalance(new BigDecimal("2000.00"));
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                new BigDecimal("2000.00"),  // Equal to balance
                null, null, null, null, null),
            null
        );
        
        // Act & Assert - should not throw
        assertThatCode(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .doesNotThrowAnyException();
    }
    
    // ========== Rule 002-3: Date Validation Tests ==========
    
    @Test
    void updateAccountAndCustomer_expirationDateBeforeOpenDate_throwsException() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        account.setOpenDate(LocalDate.of(2024, 1, 15));
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                null, null, null,
                LocalDate.of(2023, 12, 31),  // Before open date
                null, null),
            null
        );
        
        // Act & Assert
        assertThatThrownBy(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Expiration date")
            .hasMessageContaining("must be after open date");
    }
    
    @Test
    void updateAccountAndCustomer_reissueDateBeforeOpenDate_throwsException() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        account.setOpenDate(LocalDate.of(2024, 1, 15));
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                null, null, null, null,
                LocalDate.of(2024, 1, 10),  // Before open date
                null),
            null
        );
        
        // Act & Assert
        assertThatThrownBy(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Reissue date")
            .hasMessageContaining("must be on or after open date");
    }
    
    @Test
    void updateAccountAndCustomer_validDates_succeeds() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        account.setOpenDate(LocalDate.of(2024, 1, 15));
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                null, null, null,
                LocalDate.of(2027, 1, 15),  // After open date
                LocalDate.of(2024, 6, 15),  // After open date
                null),
            null
        );
        
        // Act & Assert - should not throw
        assertThatCode(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .doesNotThrowAnyException();
    }
    
    // ========== Rule 002-7: FICO Score Validation Tests ==========
    
    @ParameterizedTest
    @ValueSource(ints = {299, 0, -100, 851, 900, 1000})
    void updateAccountAndCustomer_invalidFicoScore_throwsException(int invalidScore) {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            null,
            new UpdateAccountAndCustomerRequest.CustomerUpdate(
                null, null, null, null, null, null, null, null,
                invalidScore  // Invalid FICO score
            )
        );
        
        // Act & Assert
        assertThatThrownBy(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("FICO score must be between 300 and 850");
    }
    
    @ParameterizedTest
    @ValueSource(ints = {300, 500, 720, 850})
    void updateAccountAndCustomer_validFicoScore_succeeds(int validScore) {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            null,
            new UpdateAccountAndCustomerRequest.CustomerUpdate(
                null, null, null, null, null, null, null, null,
                validScore  // Valid FICO score
            )
        );
        
        // Act & Assert - should not throw
        assertThatCode(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .doesNotThrowAnyException();
        
        // Verify FICO score updated
        Customer updatedCustomer = customerRepository.findById("000000001").orElseThrow();
        assertThat(updatedCustomer.getFicoScore()).isEqualTo(validScore);
    }
    
    // ========== Rule 002-4: Transactional Integrity Tests ==========
    
    @Test
    void updateAccountAndCustomer_validationFailure_neitherAccountNorCustomerUpdated() {
        // Arrange
        Customer customer = createSampleCustomer("000000001");
        customer.setFicoScore(700);
        customer.setAddressLine1("Original Address");
        customerRepository.save(customer);
        
        Account account = createSampleAccount("00000000001", "000000001");
        account.setCreditLimit(new BigDecimal("5000.00"));
        accountRepository.save(account);
        
        UpdateAccountAndCustomerRequest request = new UpdateAccountAndCustomerRequest(
            new UpdateAccountAndCustomerRequest.AccountUpdate(
                new BigDecimal("7500.00"),  // Valid credit limit update
                null, null, null, null, null),
            new UpdateAccountAndCustomerRequest.CustomerUpdate(
                "New Address",               // Valid address update
                null, null, null, null, null, null, null,
                900                          // Invalid FICO score (will fail)
            )
        );
        
        // Act
        assertThatThrownBy(() -> 
            accountService.updateAccountAndCustomer("00000000001", request))
            .isInstanceOf(IllegalArgumentException.class);
        
        // Assert - verify NEITHER account NOR customer were updated (Rule 002-4)
        Account unchangedAccount = accountRepository.findById("00000000001").orElseThrow();
        assertThat(unchangedAccount.getCreditLimit())
            .isEqualByComparingTo(new BigDecimal("5000.00"));  // Original value
        
        Customer unchangedCustomer = customerRepository.findById("000000001").orElseThrow();
        assertThat(unchangedCustomer.getAddressLine1())
            .isEqualTo("Original Address");  // Original value
        assertThat(unchangedCustomer.getFicoScore())
            .isEqualTo(700);  // Original value
    }
    
    // ========== Helper Methods ==========
    
    private Customer createSampleCustomer(String customerId) {
        Customer customer = new Customer();
        customer.setCustomerId(customerId);
        customer.setFirstName("John");
        customer.setMiddleName("A");
        customer.setLastName("Doe");
        customer.setAddressLine1("123 Main St");
        customer.setStateCode("IL");
        customer.setCountryCode("USA");
        customer.setZip("62701");
        customer.setPhoneNumber1("555-1234");
        customer.setSsn("123456789");
        customer.setGovernmentId("DL12345");
        customer.setDateOfBirth(LocalDate.of(1980, 5, 15));
        customer.setEftAccountId("EFT001");
        customer.setPrimaryCardHolder("Y");
        customer.setFicoScore(720);
        return customer;
    }
    
    private Account createSampleAccount(String accountId, String customerId) {
        Account account = new Account();
        account.setAccountId(accountId);
        account.setCustomerId(customerId);
        account.setActiveStatus("Y");
        account.setCurrentBalance(new BigDecimal("1250.50"));
        account.setCreditLimit(new BigDecimal("5000.00"));
        account.setCashCreditLimit(new BigDecimal("1000.00"));
        account.setOpenDate(LocalDate.of(2024, 1, 15));
        account.setExpirationDate(LocalDate.of(2027, 1, 15));
        account.setCurrentCycleCredit(BigDecimal.ZERO);
        account.setCurrentCycleDebit(BigDecimal.ZERO);
        account.setAddressZip("62701");
        account.setGroupId("STANDARD");
        return account;
    }
}
