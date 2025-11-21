package com.carddemo.poc.service;

import com.carddemo.poc.dto.LoginRequest;
import com.carddemo.poc.dto.LoginResponse;
import com.carddemo.poc.entity.User;
import com.carddemo.poc.repository.UserRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

/**
 * Authentication service implementing BR-001: User Authentication.
 * 
 * Maps to COBOL program COSGN00C which handles sign-on logic.
 * 
 * Business Rules Implemented:
 * - Rule 001: Case-insensitive credential matching (COBOL uppercase conversion)
 * - Rule 002: Both user ID and password mandatory
 * - Rule 003: User type determines routing (Admin vs Regular)
 * - Rule 004: Authentication required for all access
 * - Rule 005: Independent session context per user
 */
@Service
@Transactional
public class AuthenticationService {
    
    private static final Logger log = LoggerFactory.getLogger(AuthenticationService.class);
    private final UserRepository userRepository;
    
    public AuthenticationService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }
    
    /**
     * Authenticate user with credentials.
     * 
     * Maps to COBOL COSGN00C authentication logic (lines 200-350):
     * 1. Validate both fields provided
     * 2. Convert to uppercase (case-insensitive matching)
     * 3. Read USRSEC file
     * 4. Compare password
     * 5. Initialize COMMAREA with user info
     * 6. Route based on user type
     * 
     * FR-001.1: User Credential Validation
     * FR-001.2: Role-Based Access Differentiation
     * FR-001.3: Session Initialization
     * FR-001.4: Authentication Failure Handling
     * 
     * @param request Login credentials
     * @return LoginResponse with user info and session data
     * @throws AuthenticationException if authentication fails
     */
    public LoginResponse login(LoginRequest request) {
        log.info("Authentication attempt for user: {}", request.userId());
        
        // Rule 002: Validate both fields are provided
        // (Already validated in LoginRequest constructor)
        
        // Rule 001: Case-insensitive matching (COBOL uppercase conversion)
        String userIdUpper = request.userId().toUpperCase();
        String passwordUpper = request.password().toUpperCase();
        
        log.debug("Looking up user: {}", userIdUpper);
        
        // Read user from database (maps to EXEC CICS READ USRSEC)
        User user = userRepository.findByUserIdIgnoreCase(userIdUpper)
            .orElseThrow(() -> {
                log.warn("User not found: {}", userIdUpper);
                return new AuthenticationException("Invalid user ID or password");
            });
        
        // Verify password matches (maps to COBOL password comparison)
        if (!passwordUpper.equals(user.getPassword().toUpperCase())) {
            log.warn("Invalid password for user: {}", userIdUpper);
            throw new AuthenticationException("Invalid user ID or password");
        }
        
        // Update last login timestamp
        user.setLastLogin(LocalDateTime.now());
        userRepository.save(user);
        
        log.info("User {} authenticated successfully as type {}", user.getUserId(), user.getUserType());
        
        // Rule 003: Build response with user type for routing
        // Maps to COBOL COMMAREA initialization with user info
        return LoginResponse.success(
            user.getUserId(),
            user.getUserType(),
            user.getFirstName(),
            user.getLastName(),
            user.isAdmin()
        );
    }
    
    /**
     * Logout user (voluntary session termination).
     * 
     * Maps to COBOL F3 key handling in COSGN00C.
     * FR-001.5: Voluntary Session Termination
     * 
     * @param userId User ID to logout
     */
    public void logout(String userId) {
        log.info("User {} logged out", userId);
        // In POC, logout is handled client-side (clear session)
        // Production would invalidate JWT token or clear server-side session
    }
    
    /**
     * Custom exception for authentication failures.
     * Provides specific error messages as per FR-001.4.
     */
    public static class AuthenticationException extends RuntimeException {
        public AuthenticationException(String message) {
            super(message);
        }
    }
}
