package com.carddemo.poc.repository;

import com.carddemo.poc.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * Repository for User entities.
 * 
 * Spring Data JPA provides implementations automatically based on method names.
 * This replaces the COBOL VSAM file operations for USRSEC.
 * 
 * COBOL Mapping:
 * - findById() → EXEC CICS READ ... INTO(USRSEC-REC) ... END-EXEC
 * - existsById() → EXEC CICS READ ... checking for success
 */
@Repository
public interface UserRepository extends JpaRepository<User, String> {
    
    /**
     * Find user by ID (case-insensitive).
     * 
     * Maps to COBOL authentication logic where user IDs are converted to uppercase.
     * Spring Data JPA automatically implements this method.
     * 
     * @param userId The user ID to search for
     * @return Optional containing the user if found
     */
    Optional<User> findByUserIdIgnoreCase(String userId);
    
    /**
     * Check if a user exists with the given ID (case-insensitive).
     * 
     * @param userId The user ID to check
     * @return true if user exists, false otherwise
     */
    boolean existsByUserIdIgnoreCase(String userId);
}
