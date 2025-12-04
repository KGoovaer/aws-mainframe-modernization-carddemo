package com.carddemo.poc.service;

import com.carddemo.poc.dto.LoginRequest;
import com.carddemo.poc.dto.LoginResponse;
import com.carddemo.poc.entity.User;
import com.carddemo.poc.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for AuthenticationService.
 * 
 * Tests all business requirements from BR-001: User Authentication.
 * Uses in-memory H2 database for testing.
 */
@DataJpaTest
@Import(AuthenticationService.class)
class AuthenticationServiceTest {
    
    @Autowired
    private AuthenticationService authenticationService;
    
    @Autowired
    private UserRepository userRepository;
    
    @BeforeEach
    void setUp() {
        userRepository.deleteAll();
    }
    
    /**
     * Test: US-001 - Successful Login with Valid Credentials
     * BR-001, FR-001.1, FR-001.3
     * 
     * COBOL: COSGN00C lines 200-310 (successful authentication flow)
     */
    @Test
    void login_validCredentials_returnsSuccessResponse() {
        // Arrange - Create test user
        User user = new User("ADMIN01", "ADMIN01", "A");
        user.setFirstName("System");
        user.setLastName("Administrator");
        user.setCreatedAt(LocalDateTime.now());
        userRepository.save(user);
        
        LoginRequest request = new LoginRequest("ADMIN01", "ADMIN01");
        
        // Act
        LoginResponse response = authenticationService.login(request);
        
        // Assert
        assertThat(response).isNotNull();
        assertThat(response.userId()).isEqualTo("ADMIN01");
        assertThat(response.userType()).isEqualTo("A");
        assertThat(response.firstName()).isEqualTo("System");
        assertThat(response.lastName()).isEqualTo("Administrator");
        assertThat(response.isAdmin()).isTrue();
        assertThat(response.message()).isEqualTo("Login successful");
        
        // Verify last login timestamp was updated
        User updatedUser = userRepository.findById("ADMIN01").orElseThrow();
        assertThat(updatedUser.getLastLogin()).isNotNull();
    }
    
    /**
     * Test: US-002 - Login Failure with Invalid Password
     * BR-001, FR-001.4, Rule 002
     * 
     * COBOL: COSGN00C lines 350-400 (password mismatch handling)
     */
    @Test
    void login_invalidPassword_throwsAuthenticationException() {
        // Arrange
        User user = new User("USER01", "USER01", "U");
        user.setFirstName("John");
        user.setLastName("Doe");
        userRepository.save(user);
        
        LoginRequest request = new LoginRequest("USER01", "WRONGPASS");
        
        // Act & Assert
        assertThatThrownBy(() -> authenticationService.login(request))
            .isInstanceOf(AuthenticationService.AuthenticationException.class)
            .hasMessageContaining("Invalid user ID or password");
    }
    
    /**
     * Test: US-003 - Login Failure with Non-Existent Username
     * BR-001, FR-001.4
     * 
     * COBOL: COSGN00C lines 320-350 (user not found handling)
     */
    @Test
    void login_nonExistentUser_throwsAuthenticationException() {
        // Arrange
        LoginRequest request = new LoginRequest("NOUSER", "PASSWORD");
        
        // Act & Assert
        assertThatThrownBy(() -> authenticationService.login(request))
            .isInstanceOf(AuthenticationService.AuthenticationException.class)
            .hasMessageContaining("Invalid user ID or password");
    }
    
    /**
     * Test: Rule 001 - Credential Case Insensitivity
     * BR-001, Rule 001
     * 
     * COBOL: COSGN00C lines 240-250 (uppercase conversion)
     */
    @Test
    void login_mixedCaseCredentials_authenticatesSuccessfully() {
        // Arrange - Store user with uppercase credentials
        User user = new User("ADMIN01", "ADMIN01", "A");
        user.setFirstName("Admin");
        user.setLastName("User");
        userRepository.save(user);
        
        // Act - Login with mixed case
        LoginRequest request = new LoginRequest("admin01", "admin01");
        LoginResponse response = authenticationService.login(request);
        
        // Assert
        assertThat(response).isNotNull();
        assertThat(response.userId()).isEqualTo("ADMIN01");
    }
    
    /**
     * Test: US-008 - Role-Based Login Routing
     * BR-001, FR-001.2, Rule 003
     * 
     * COBOL: COSGN00C lines 280-310 (user type routing)
     */
    @Test
    void login_adminUser_returnsAdminFlag() {
        // Arrange
        User admin = new User("ADMIN01", "ADMIN01", "A");
        admin.setFirstName("Admin");
        admin.setLastName("User");
        userRepository.save(admin);
        
        LoginRequest request = new LoginRequest("ADMIN01", "ADMIN01");
        
        // Act
        LoginResponse response = authenticationService.login(request);
        
        // Assert
        assertThat(response.userType()).isEqualTo("A");
        assertThat(response.isAdmin()).isTrue();
    }
    
    /**
     * Test: Regular user login
     * BR-001, FR-001.2, Rule 003
     */
    @Test
    void login_regularUser_returnsUserFlag() {
        // Arrange
        User user = new User("USER01", "USER01", "U");
        user.setFirstName("Regular");
        user.setLastName("User");
        userRepository.save(user);
        
        LoginRequest request = new LoginRequest("USER01", "USER01");
        
        // Act
        LoginResponse response = authenticationService.login(request);
        
        // Assert
        assertThat(response.userType()).isEqualTo("U");
        assertThat(response.isAdmin()).isFalse();
    }
    
    /**
     * Test: US-011 - Missing Required Fields Validation
     * BR-001, FR-001.4, Rule 002
     * 
     * COBOL: COSGN00C lines 220-235 (field validation)
     */
    @Test
    void login_missingUserId_throwsIllegalArgumentException() {
        // Act & Assert
        assertThatThrownBy(() -> new LoginRequest("", "password"))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("User ID is required");
    }
    
    /**
     * Test: Missing password validation
     * BR-001, FR-001.4, Rule 002
     */
    @Test
    void login_missingPassword_throwsIllegalArgumentException() {
        // Act & Assert
        assertThatThrownBy(() -> new LoginRequest("USER01", ""))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Password is required");
    }
    
    /**
     * Test: US-005 - User Logout
     * BR-001, FR-001.5
     * 
     * COBOL: F3 key handling in COSGN00C
     */
    @Test
    void logout_validUser_completesSuccessfully() {
        // Arrange
        String userId = "USER01";
        
        // Act - Should not throw exception
        assertThatCode(() -> authenticationService.logout(userId))
            .doesNotThrowAnyException();
    }
}
