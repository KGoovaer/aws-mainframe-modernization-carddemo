package com.carddemo.poc.controller;

import com.carddemo.poc.dto.LoginRequest;
import com.carddemo.poc.dto.LoginResponse;
import com.carddemo.poc.service.AuthenticationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * REST controller for authentication operations.
 * 
 * Provides API endpoints for Angular frontend to handle user authentication.
 * Maps to COBOL program COSGN00C functionality.
 */
@RestController
@RequestMapping("/api/auth")
@CrossOrigin(origins = "http://localhost:4200")
public class AuthenticationController {
    
    private static final Logger log = LoggerFactory.getLogger(AuthenticationController.class);
    private final AuthenticationService authenticationService;
    
    public AuthenticationController(AuthenticationService authenticationService) {
        this.authenticationService = authenticationService;
    }
    
    /**
     * Login endpoint.
     * 
     * POST /api/auth/login
     * 
     * Request body:
     * {
     *   "userId": "ADMIN01",
     *   "password": "ADMIN01"
     * }
     * 
     * Success response (200):
     * {
     *   "userId": "ADMIN01",
     *   "userType": "A",
     *   "firstName": "System",
     *   "lastName": "Administrator",
     *   "isAdmin": true,
     *   "message": "Login successful"
     * }
     * 
     * Error response (401):
     * {
     *   "error": "Invalid user ID or password"
     * }
     * 
     * Maps to COBOL COSGN00C authentication flow.
     * 
     * @param request Login credentials
     * @return LoginResponse with user info or error
     */
    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody LoginRequest request) {
        log.info("Login request received for user: {}", request.userId());
        
        try {
            LoginResponse response = authenticationService.login(request);
            log.info("Login successful for user: {}", request.userId());
            return ResponseEntity.ok(response);
            
        } catch (AuthenticationService.AuthenticationException ex) {
            log.warn("Login failed for user {}: {}", request.userId(), ex.getMessage());
            return ResponseEntity
                .status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("error", ex.getMessage()));
                
        } catch (IllegalArgumentException ex) {
            log.warn("Invalid login request: {}", ex.getMessage());
            return ResponseEntity
                .badRequest()
                .body(Map.of("error", ex.getMessage()));
        }
    }
    
    /**
     * Logout endpoint.
     * 
     * POST /api/auth/logout
     * 
     * Request body:
     * {
     *   "userId": "ADMIN01"
     * }
     * 
     * Success response (200):
     * {
     *   "message": "Logout successful"
     * }
     * 
     * Maps to COBOL F3 key handling (voluntary session termination).
     * 
     * @param request Map containing userId
     * @return Success message
     */
    @PostMapping("/logout")
    public ResponseEntity<?> logout(@RequestBody Map<String, String> request) {
        String userId = request.get("userId");
        log.info("Logout request received for user: {}", userId);
        
        authenticationService.logout(userId);
        
        return ResponseEntity.ok(Map.of("message", "Logout successful"));
    }
    
    /**
     * Health check endpoint.
     * 
     * GET /api/auth/health
     * 
     * @return Simple status message
     */
    @GetMapping("/health")
    public ResponseEntity<?> health() {
        return ResponseEntity.ok(Map.of(
            "status", "UP",
            "service", "authentication"
        ));
    }
}
