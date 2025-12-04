package com.carddemo.poc.controller;

import com.carddemo.poc.dto.AccountDetailsDto;
import com.carddemo.poc.dto.UpdateAccountAndCustomerRequest;
import com.carddemo.poc.service.AccountService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST controller for account management operations.
 * Provides endpoints for account inquiry and updates.
 */
@RestController
@RequestMapping("/api/accounts")
@CrossOrigin(origins = "http://localhost:4200")
public class AccountController {
    
    private static final Logger log = LoggerFactory.getLogger(AccountController.class);
    
    private final AccountService accountService;
    
    public AccountController(AccountService accountService) {
        this.accountService = accountService;
    }
    
    /**
     * Get account details with customer information (FR-002.1).
     * 
     * @param accountId Account ID (11 digits)
     * @return Account details with customer info
     */
    @GetMapping("/{accountId}")
    public ResponseEntity<?> getAccountDetails(@PathVariable String accountId) {
        try {
            log.info("GET /api/accounts/{}", accountId);
            AccountDetailsDto account = accountService.getAccountDetails(accountId);
            return ResponseEntity.ok(account);
        } catch (IllegalArgumentException ex) {
            log.warn("Invalid request for account {}: {}", accountId, ex.getMessage());
            return ResponseEntity
                .badRequest()
                .body(new ErrorResponse(ex.getMessage()));
        } catch (Exception ex) {
            log.error("Error retrieving account " + accountId, ex);
            return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(new ErrorResponse("Internal server error"));
        }
    }
    
    /**
     * Update account and customer information (FR-002.2).
     * 
     * @param accountId Account ID (11 digits)
     * @param request Update request with account and customer fields
     * @return Success or error response
     */
    @PutMapping("/{accountId}")
    public ResponseEntity<?> updateAccountAndCustomer(
            @PathVariable String accountId,
            @RequestBody UpdateAccountAndCustomerRequest request) {
        try {
            log.info("PUT /api/accounts/{}", accountId);
            accountService.updateAccountAndCustomer(accountId, request);
            return ResponseEntity.ok(new SuccessResponse("Account and customer updated successfully"));
        } catch (IllegalArgumentException ex) {
            log.warn("Validation error for account {}: {}", accountId, ex.getMessage());
            return ResponseEntity
                .badRequest()
                .body(new ErrorResponse(ex.getMessage()));
        } catch (IllegalStateException ex) {
            log.warn("Account or customer not found: {}", ex.getMessage());
            return ResponseEntity
                .status(HttpStatus.NOT_FOUND)
                .body(new ErrorResponse(ex.getMessage()));
        } catch (Exception ex) {
            log.error("Error updating account " + accountId, ex);
            return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(new ErrorResponse("Internal server error"));
        }
    }
    
    /**
     * Health check endpoint for account service.
     */
    @GetMapping("/health")
    public ResponseEntity<HealthResponse> health() {
        return ResponseEntity.ok(
            new HealthResponse("UP", "account-management"));
    }
    
    // Response DTOs
    
    record ErrorResponse(String error) {}
    
    record SuccessResponse(String message) {}
    
    record HealthResponse(String status, String service) {}
}
