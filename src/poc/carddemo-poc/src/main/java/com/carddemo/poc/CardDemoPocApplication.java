package com.carddemo.poc;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Main Spring Boot application for CardDemo POC.
 * 
 * This POC implements a simplified version of the CardDemo mainframe application
 * to validate business logic and data models before building the production system.
 */
@SpringBootApplication
public class CardDemoPocApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoPocApplication.class, args);
    }
}
