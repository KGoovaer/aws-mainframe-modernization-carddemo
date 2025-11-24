import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AuthService } from '../../services/auth.service';

/**
 * Login Component
 * 
 * Implements User Story US-001: Successful Login
 * Maps to COBOL program COSGN00C (Sign-On Screen)
 * 
 * Features:
 * - User ID and password input (FR-001.1)
 * - Case-insensitive credentials (Rule 001)
 * - Authentication validation (FR-001.3, FR-001.4)
 * - Role-based routing (FR-001.2, Rule 003)
 */
@Component({
  selector: 'app-login',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  userId = '';
  password = '';
  errorMessage = '';
  isLoading = false;

  constructor(
    private authService: AuthService,
    private router: Router
  ) {}

  ngOnInit(): void {
    // Redirect if already logged in
    if (this.authService.isAuthenticated) {
      this.router.navigate(['/menu']);
    }
  }

  /**
   * Handle login form submission
   * US-001: Successful Login with Valid Credentials
   * COBOL: COSGN00C lines 200-310
   */
  onLogin(): void {
    // Clear previous errors
    this.errorMessage = '';

    // Validate inputs (FR-001.1)
    if (!this.userId.trim()) {
      this.errorMessage = 'User ID is required';
      return;
    }

    if (!this.password.trim()) {
      this.errorMessage = 'Password is required';
      return;
    }

    // Attempt login
    this.isLoading = true;
    
    this.authService.login(this.userId, this.password).subscribe({
      next: (response) => {
        // Login successful - navigate to main menu
        // FR-001.2: Navigate based on user type
        this.router.navigate(['/menu']);
      },
      error: (error) => {
        // Login failed - show error message
        // FR-001.4: Display authentication error
        this.errorMessage = error.message;
        this.isLoading = false;
        
        // Clear password field for security
        this.password = '';
      },
      complete: () => {
        this.isLoading = false;
      }
    });
  }

  /**
   * Handle clear button (F5)
   * COBOL: F5 key handling in COSGN00C
   */
  onClear(): void {
    this.userId = '';
    this.password = '';
    this.errorMessage = '';
  }

  /**
   * Check if form is valid
   */
  isFormValid(): boolean {
    return this.userId.trim().length > 0 && this.password.trim().length > 0;
  }
}
