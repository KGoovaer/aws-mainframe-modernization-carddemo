package com.carddemo.poc.dto;

/**
 * Request DTO for user login.
 * 
 * Maps to COBOL sign-on screen (COSGN00) input fields.
 */
public record LoginRequest(
    String userId,
    String password
) {
    /**
     * Compact constructor for validation.
     */
    public LoginRequest {
        if (userId == null || userId.isBlank()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (password == null || password.isBlank()) {
            throw new IllegalArgumentException("Password is required");
        }
    }
}
