import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { AccountDetails, UpdateAccountRequest } from '../models/account.model';

/**
 * Account Service
 * 
 * Handles account management operations and communication
 * with the backend account API.
 * 
 * Maps to backend AccountService and AccountController (Spring Boot)
 * 
 * Implements:
 * - FR-002.1: Account Inquiry
 * - FR-002.2: Account/Customer Update
 */
@Injectable({
  providedIn: 'root'
})
export class AccountService {
  private readonly API_URL = 'http://localhost:8080/api/accounts';

  constructor(private http: HttpClient) {}

  /**
   * Get account details by account ID
   * 
   * FR-002.1: Account Inquiry
   * Maps to COBOL CBACT01C (Account View)
   * 
   * @param accountId 11-digit account ID
   * @returns Observable<AccountDetails> Account and customer information
   */
  getAccount(accountId: string): Observable<AccountDetails> {
    return this.http.get<AccountDetails>(`${this.API_URL}/${accountId}`).pipe(
      catchError(this.handleError)
    );
  }

  /**
   * Update account and/or customer information
   * 
   * FR-002.2: Account/Customer Update
   * Maps to COBOL CBACT02C (Account Update)
   * 
   * Enforces business rules:
   * - Rule 002-2: Credit limit validation
   * - Rule 002-3: Date validation
   * - Rule 002-7: FICO score validation (300-850)
   * - Rule 002-4: Transactional integrity
   * 
   * @param accountId 11-digit account ID
   * @param request Update request with optional account/customer updates
   * @returns Observable<AccountDetails> Updated account details
   */
  updateAccount(accountId: string, request: UpdateAccountRequest): Observable<AccountDetails> {
    return this.http.put<AccountDetails>(`${this.API_URL}/${accountId}`, request).pipe(
      catchError(this.handleError)
    );
  }

  /**
   * Health check endpoint
   * 
   * @returns Observable<string> Health status message
   */
  healthCheck(): Observable<string> {
    return this.http.get(`${this.API_URL}/health`, { responseType: 'text' }).pipe(
      catchError(this.handleError)
    );
  }

  /**
   * Handle HTTP errors
   * 
   * Provides user-friendly error messages based on HTTP status codes:
   * - 400: Validation error (invalid input)
   * - 404: Account not found
   * - 500: Server error
   * - 0: Connection error
   */
  private handleError(error: HttpErrorResponse) {
    let errorMessage = 'An error occurred';
    
    if (error.error instanceof ErrorEvent) {
      // Client-side error
      errorMessage = `Error: ${error.error.message}`;
    } else {
      // Server-side error
      if (error.status === 400) {
        // Validation error - extract message from backend
        errorMessage = error.error || 'Invalid input. Please check your data.';
      } else if (error.status === 404) {
        errorMessage = error.error || 'Account not found.';
      } else if (error.status === 500) {
        errorMessage = error.error || 'Server error occurred.';
      } else if (error.status === 0) {
        errorMessage = 'Unable to connect to server. Please check if the backend is running.';
      } else {
        errorMessage = error.error || `Unexpected error: ${error.status}`;
      }
    }
    
    return throwError(() => new Error(errorMessage));
  }
}
