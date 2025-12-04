package com.carddemo.poc.dto;

import java.time.LocalDate;

/**
 * Customer information DTO.
 */
public record CustomerDto(
    String customerId,
    String firstName,
    String middleName,
    String lastName,
    String addressLine1,
    String addressLine2,
    String addressLine3,
    String stateCode,
    String countryCode,
    String zip,
    String phoneNumber1,
    String phoneNumber2,
    String ssn,
    String governmentId,
    LocalDate dateOfBirth,
    String eftAccountId,
    String primaryCardHolder,
    Integer ficoScore
) {
}
