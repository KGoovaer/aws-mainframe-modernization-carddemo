package com.carddemo.poc.repository;

import com.carddemo.poc.entity.Customer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * Repository for Customer entities.
 * Spring Data JPA provides implementations automatically.
 */
@Repository
public interface CustomerRepository extends JpaRepository<Customer, String> {
    
    /**
     * Find customer by SSN.
     */
    Optional<Customer> findBySsn(String ssn);
}
