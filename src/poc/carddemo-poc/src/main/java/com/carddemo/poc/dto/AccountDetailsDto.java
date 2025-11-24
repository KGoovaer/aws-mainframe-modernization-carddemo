package com.carddemo.poc.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Response DTO for account details with customer information.
 */
public record AccountDetailsDto(
    String accountId,
    String customerId,
    String activeStatus,
    BigDecimal currentBalance,
    BigDecimal creditLimit,
    BigDecimal cashCreditLimit,
    LocalDate openDate,
    LocalDate expirationDate,
    LocalDate reissueDate,
    BigDecimal currentCycleCredit,
    BigDecimal currentCycleDebit,
    String addressZip,
    String groupId,
    CustomerDto customer
) {
}
