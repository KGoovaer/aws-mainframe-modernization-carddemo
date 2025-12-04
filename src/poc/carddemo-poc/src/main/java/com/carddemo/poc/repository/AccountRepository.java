package com.carddemo.poc.repository;

import com.carddemo.poc.entity.Account;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository for Account entities.
 * Spring Data JPA provides implementations automatically.
 */
@Repository
public interface AccountRepository extends JpaRepository<Account, String> {
    
    /**
     * Find accounts by customer ID.
     */
    List<Account> findByCustomerId(String customerId);
    
    /**
     * Find accounts by active status.
     */
    List<Account> findByActiveStatus(String activeStatus);
    
    /**
     * Find account with customer eagerly loaded (optimized for inquiry).
     */
    @Query("SELECT a FROM Account a LEFT JOIN FETCH a.customer WHERE a.accountId = :accountId")
    Optional<Account> findByIdWithCustomer(@Param("accountId") String accountId);
}
