package com.carddemo.poc.dto;

/**
 * Response DTO for successful login.
 * 
 * Contains user information and session data needed by the frontend.
 * Maps to COBOL COMMAREA structure (COCOM01Y) that holds session state.
 */
public record LoginResponse(
    String userId,
    String userType,
    String firstName,
    String lastName,
    boolean isAdmin,
    String message
) {
    /**
     * Create success response for authenticated user.
     */
    public static LoginResponse success(String userId, String userType, String firstName, String lastName, boolean isAdmin) {
        return new LoginResponse(
            userId,
            userType,
            firstName,
            lastName,
            isAdmin,
            "Login successful"
        );
    }
}
