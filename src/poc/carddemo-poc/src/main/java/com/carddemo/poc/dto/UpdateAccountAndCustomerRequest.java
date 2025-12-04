package com.carddemo.poc.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Request DTO for updating account and customer information.
 */
public record UpdateAccountAndCustomerRequest(
    AccountUpdate account,
    CustomerUpdate customer
) {
    /**
     * Account fields to update.
     */
    public record AccountUpdate(
        BigDecimal creditLimit,
        BigDecimal cashCreditLimit,
        String activeStatus,
        LocalDate expirationDate,
        LocalDate reissueDate,
        String groupId
    ) {
    }
    
    /**
     * Customer fields to update.
     */
    public record CustomerUpdate(
        String addressLine1,
        String addressLine2,
        String addressLine3,
        String stateCode,
        String countryCode,
        String zip,
        String phoneNumber1,
        String phoneNumber2,
        Integer ficoScore
    ) {
    }
}
