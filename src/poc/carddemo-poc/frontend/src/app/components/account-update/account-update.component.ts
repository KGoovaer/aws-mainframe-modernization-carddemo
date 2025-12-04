import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { AccountService } from '../../services/account.service';
import { AccountDetails, UpdateAccountRequest } from '../../models/account.model';

/**
 * Account Update Component
 * 
 * Implements FR-002.2: Account/Customer Update
 * Maps to COBOL program CBACT02C and COACTUPC (Account Update Screen)
 * 
 * Features:
 * - Load existing account and customer data
 * - Update account information (credit limits, dates, status)
 * - Update customer information (address, phone, FICO score)
 * - Enforce business rules with validation
 * - Transactional integrity (all or nothing update)
 * 
 * Business Rules Enforced:
 * - Rule 002-2: Credit limit validation
 * - Rule 002-3: Date validation
 * - Rule 002-7: FICO score validation (300-850)
 * - Rule 002-4: Transactional integrity
 */
@Component({
  selector: 'app-account-update',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './account-update.component.html',
  styleUrls: ['./account-update.component.css']
})
export class AccountUpdateComponent implements OnInit {
  accountId: string = '';
  account: AccountDetails | null = null;
  loading: boolean = false;
  errorMessage: string = '';
  successMessage: string = '';
  
  // Form fields - Account
  creditLimit: number = 0;
  cashCreditLimit: number = 0;
  activeStatus: string = '';
  expirationDate: string = '';
  reissueDate: string = '';
  groupId: string = '';
  
  // Form fields - Customer
  addressLine1: string = '';
  addressLine2: string = '';
  addressLine3: string = '';
  stateCode: string = '';
  countryCode: string = '';
  zip: string = '';
  phoneNumber1: string = '';
  phoneNumber2: string = '';
  ficoScore: number = 0;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private accountService: AccountService
  ) {}

  ngOnInit(): void {
    // Get account ID from route parameter
    this.accountId = this.route.snapshot.paramMap.get('id') || '';
    
    if (this.accountId) {
      this.loadAccount();
    } else {
      this.errorMessage = 'No account ID provided';
    }
  }

  /**
   * Load account details
   */
  loadAccount(): void {
    this.loading = true;
    this.errorMessage = '';
    
    this.accountService.getAccount(this.accountId).subscribe({
      next: (data) => {
        this.account = data;
        this.populateForm();
        this.loading = false;
      },
      error: (error) => {
        this.errorMessage = error.message;
        this.loading = false;
      }
    });
  }

  /**
   * Populate form with account data
   */
  populateForm(): void {
    if (!this.account) return;
    
    // Account fields
    this.creditLimit = this.account.creditLimit;
    this.cashCreditLimit = this.account.cashCreditLimit;
    this.activeStatus = this.account.activeStatus;
    this.expirationDate = this.account.expirationDate || '';
    this.reissueDate = this.account.reissueDate || '';
    this.groupId = this.account.groupId || '';
    
    // Customer fields
    this.addressLine1 = this.account.customer.addressLine1;
    this.addressLine2 = this.account.customer.addressLine2 || '';
    this.addressLine3 = this.account.customer.addressLine3 || '';
    this.stateCode = this.account.customer.stateCode;
    this.countryCode = this.account.customer.countryCode;
    this.zip = this.account.customer.zip;
    this.phoneNumber1 = this.account.customer.phoneNumber1;
    this.phoneNumber2 = this.account.customer.phoneNumber2 || '';
    this.ficoScore = this.account.customer.ficoScore;
  }

  /**
   * Validate form inputs
   * Client-side validation before sending to backend
   */
  validateForm(): string | null {
    // Rule 002-2: Credit limit validation
    if (this.creditLimit < 0 || this.creditLimit > 9999999999.99) {
      return 'Credit limit must be between 0 and 9,999,999,999.99';
    }
    
    if (this.cashCreditLimit < 0 || this.cashCreditLimit > this.creditLimit) {
      return 'Cash credit limit must be between 0 and credit limit';
    }
    
    // Current balance check (Rule 002-2)
    if (this.account && this.creditLimit < this.account.currentBalance) {
      return `Credit limit cannot be less than current balance (${this.formatCurrency(this.account.currentBalance)})`;
    }
    
    // Rule 002-3: Date validation
    if (this.expirationDate && this.account) {
      const expDate = new Date(this.expirationDate);
      const openDate = new Date(this.account.openDate);
      
      if (expDate <= openDate) {
        return 'Expiration date must be after open date';
      }
    }
    
    if (this.reissueDate && this.account) {
      const reisDate = new Date(this.reissueDate);
      const openDate = new Date(this.account.openDate);
      
      if (reisDate < openDate) {
        return 'Reissue date cannot be before open date';
      }
    }
    
    // Rule 002-7: FICO score validation
    if (this.ficoScore < 300 || this.ficoScore > 850) {
      return 'FICO score must be between 300 and 850';
    }
    
    // Required fields
    if (!this.addressLine1.trim()) {
      return 'Address Line 1 is required';
    }
    
    if (!this.stateCode.trim()) {
      return 'State code is required';
    }
    
    if (!this.countryCode.trim()) {
      return 'Country code is required';
    }
    
    if (!this.zip.trim()) {
      return 'ZIP code is required';
    }
    
    if (!this.phoneNumber1.trim()) {
      return 'Phone number 1 is required';
    }
    
    return null; // Validation passed
  }

  /**
   * Submit update request
   * FR-002.2: Account/Customer Update
   */
  onUpdate(): void {
    this.errorMessage = '';
    this.successMessage = '';
    
    // Client-side validation
    const validationError = this.validateForm();
    if (validationError) {
      this.errorMessage = validationError;
      return;
    }
    
    // Build update request
    const request: UpdateAccountRequest = {
      accountUpdate: {
        creditLimit: this.creditLimit,
        cashCreditLimit: this.cashCreditLimit,
        activeStatus: this.activeStatus,
        expirationDate: this.expirationDate || undefined,
        reissueDate: this.reissueDate || undefined,
        groupId: this.groupId || undefined
      },
      customerUpdate: {
        addressLine1: this.addressLine1,
        addressLine2: this.addressLine2 || undefined,
        addressLine3: this.addressLine3 || undefined,
        stateCode: this.stateCode,
        countryCode: this.countryCode,
        zip: this.zip,
        phoneNumber1: this.phoneNumber1,
        phoneNumber2: this.phoneNumber2 || undefined,
        ficoScore: this.ficoScore
      }
    };
    
    // Send update request
    this.loading = true;
    this.accountService.updateAccount(this.accountId, request).subscribe({
      next: (data) => {
        this.account = data;
        this.populateForm(); // Refresh form with updated data
        this.successMessage = 'Account and customer updated successfully';
        this.loading = false;
      },
      error: (error) => {
        this.errorMessage = error.message;
        this.loading = false;
      }
    });
  }

  /**
   * Cancel and return to account view
   */
  onCancel(): void {
    this.router.navigate(['/accounts']);
  }

  /**
   * Return to main menu
   */
  onReturn(): void {
    this.router.navigate(['/menu']);
  }

  /**
   * Format currency values
   */
  formatCurrency(value: number): string {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD'
    }).format(value);
  }

  /**
   * Format date for display
   */
  formatDate(dateString: string | null): string {
    if (!dateString) return 'N/A';
    const date = new Date(dateString);
    return date.toLocaleDateString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit'
    });
  }
}
