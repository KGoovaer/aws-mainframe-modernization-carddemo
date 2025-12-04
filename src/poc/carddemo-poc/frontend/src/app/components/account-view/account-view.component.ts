import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AccountService } from '../../services/account.service';
import { AccountDetails } from '../../models/account.model';

/**
 * Account View Component
 * 
 * Implements FR-002.1: Account Inquiry
 * Maps to COBOL program CBACT01C and COACTVWC (Account View Screen)
 * 
 * Features:
 * - Search account by account ID
 * - Display account details
 * - Display customer information
 * - Navigate to account update screen
 * - Business Rule 002-1: 11-digit account ID validation
 */
@Component({
  selector: 'app-account-view',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './account-view.component.html',
  styleUrls: ['./account-view.component.css']
})
export class AccountViewComponent {
  accountId: string = '';
  account: AccountDetails | null = null;
  loading: boolean = false;
  errorMessage: string = '';
  successMessage: string = '';

  constructor(
    private accountService: AccountService,
    private router: Router
  ) {}

  /**
   * Search for account by ID
   * FR-002.1: Account Inquiry
   * Rule 002-1: Account ID must be 11 numeric digits
   */
  onSearch(): void {
    // Clear previous messages
    this.errorMessage = '';
    this.successMessage = '';
    this.account = null;

    // Trim whitespace
    this.accountId = this.accountId.trim();

    // Validate account ID format (Rule 002-1)
    if (!this.accountId) {
      this.errorMessage = 'Please enter an account ID';
      return;
    }

    if (!/^\d{11}$/.test(this.accountId)) {
      this.errorMessage = 'Account ID must be exactly 11 numeric digits';
      return;
    }

    // Fetch account details
    this.loading = true;
    this.accountService.getAccount(this.accountId).subscribe({
      next: (data) => {
        this.account = data;
        this.successMessage = 'Account retrieved successfully';
        this.loading = false;
      },
      error: (error) => {
        this.errorMessage = error.message;
        this.loading = false;
      }
    });
  }

  /**
   * Navigate to account update screen
   */
  onUpdate(): void {
    if (this.account) {
      this.router.navigate(['/accounts', this.account.accountId, 'edit']);
    }
  }

  /**
   * Clear search and results
   */
  onClear(): void {
    this.accountId = '';
    this.account = null;
    this.errorMessage = '';
    this.successMessage = '';
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
   * Format date values
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

  /**
   * Get account status label
   */
  getAccountStatusLabel(status: string): string {
    return status === 'Y' ? 'ACTIVE' : 'INACTIVE';
  }

  /**
   * Get account status CSS class
   */
  getAccountStatusClass(status: string): string {
    return status === 'Y' ? 'status-active' : 'status-inactive';
  }

  /**
   * Format phone number
   */
  formatPhone(phone: string | null): string {
    if (!phone) return 'N/A';
    // Format as (XXX) XXX-XXXX if 10 digits
    if (phone.length === 10) {
      return `(${phone.substring(0, 3)}) ${phone.substring(3, 6)}-${phone.substring(6)}`;
    }
    return phone;
  }

  /**
   * Format SSN (masked)
   */
  formatSSN(ssn: string): string {
    if (ssn.length === 9) {
      return `***-**-${ssn.substring(5)}`;
    }
    return '***-**-****';
  }
}
