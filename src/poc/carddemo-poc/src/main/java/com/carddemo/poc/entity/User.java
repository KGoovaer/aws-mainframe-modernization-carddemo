package com.carddemo.poc.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * User entity - maps to COBOL USRSEC file.
 * 
 * Represents a user security profile for authentication and authorization.
 * 
 * COBOL Mapping:
 * - User ID: USRSEC-ID PIC X(8)
 * - Password: USRSEC-PWD PIC X(8)
 * - User Type: USRSEC-TYPE PIC X (A=Admin, U=User)
 * - Additional fields from CSUSR01Y copybook
 */
@Entity
@Table(name = "users")
public class User {
    
    /**
     * User ID - unique identifier for the user.
     * Maps to COBOL USRSEC-ID PIC X(8)
     */
    @Id
    @Column(name = "user_id", length = 8, nullable = false)
    private String userId;
    
    /**
     * Password - user's authentication credential.
     * Maps to COBOL USRSEC-PWD PIC X(8)
     * 
     * Note: In COBOL, passwords are stored as uppercase text (not hashed).
     * For POC, we maintain this simple approach.
     * For production, this should be hashed using bcrypt.
     */
    @Column(name = "password", length = 8, nullable = false)
    private String password;
    
    /**
     * User type - role designation.
     * Maps to COBOL USRSEC-TYPE PIC X
     * 
     * Valid values:
     * - 'A' = Administrator
     * - 'U' = Regular User
     */
    @Column(name = "user_type", length = 1, nullable = false)
    private String userType;
    
    /**
     * First name of the user.
     */
    @Column(name = "first_name", length = 20)
    private String firstName;
    
    /**
     * Last name of the user.
     */
    @Column(name = "last_name", length = 20)
    private String lastName;
    
    /**
     * When the user account was created.
     */
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    /**
     * When the user last logged in.
     */
    @Column(name = "last_login")
    private LocalDateTime lastLogin;
    
    // Constructors
    
    public User() {
    }
    
    public User(String userId, String password, String userType) {
        this.userId = userId;
        this.password = password;
        this.userType = userType;
        this.createdAt = LocalDateTime.now();
    }
    
    // Getters and Setters
    
    public String getUserId() {
        return userId;
    }
    
    public void setUserId(String userId) {
        this.userId = userId;
    }
    
    public String getPassword() {
        return password;
    }
    
    public void setPassword(String password) {
        this.password = password;
    }
    
    public String getUserType() {
        return userType;
    }
    
    public void setUserType(String userType) {
        this.userType = userType;
    }
    
    public String getFirstName() {
        return firstName;
    }
    
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }
    
    public String getLastName() {
        return lastName;
    }
    
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
    
    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    
    public LocalDateTime getLastLogin() {
        return lastLogin;
    }
    
    public void setLastLogin(LocalDateTime lastLogin) {
        this.lastLogin = lastLogin;
    }
    
    /**
     * Helper method to check if user is an administrator.
     * Maps to COBOL logic: IF USRSEC-TYPE = 'A'
     */
    public boolean isAdmin() {
        return "A".equalsIgnoreCase(userType);
    }
    
    /**
     * Helper method to check if user is a regular user.
     * Maps to COBOL logic: IF USRSEC-TYPE = 'U'
     */
    public boolean isRegularUser() {
        return "U".equalsIgnoreCase(userType);
    }
    
    @Override
    public String toString() {
        return "User{" +
                "userId='" + userId + '\'' +
                ", userType='" + userType + '\'' +
                ", firstName='" + firstName + '\'' +
                ", lastName='" + lastName + '\'' +
                '}';
    }
}
