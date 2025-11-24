import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { BehaviorSubject, Observable, throwError } from 'rxjs';
import { tap, catchError } from 'rxjs/operators';
import { LoginRequest, LoginResponse, UserSession } from '../models/auth.model';

/**
 * Authentication Service
 * 
 * Handles user authentication, session management, and communication
 * with the backend authentication API.
 * 
 * Maps to backend AuthenticationService (Spring Boot)
 */
@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private readonly API_URL = 'http://localhost:8080/api/auth';
  
  private currentUserSubject = new BehaviorSubject<UserSession | null>(null);
  public currentUser$ = this.currentUserSubject.asObservable();

  constructor(private http: HttpClient) {
    // Load user from session storage on startup
    const storedUser = sessionStorage.getItem('currentUser');
    if (storedUser) {
      this.currentUserSubject.next(JSON.parse(storedUser));
    }
  }

  /**
   * Get current user session
   */
  get currentUserValue(): UserSession | null {
    return this.currentUserSubject.value;
  }

  /**
   * Check if user is authenticated
   */
  get isAuthenticated(): boolean {
    return this.currentUserValue !== null;
  }

  /**
   * Check if current user is admin
   */
  get isAdmin(): boolean {
    return this.currentUserValue?.isAdmin ?? false;
  }

  /**
   * Login user with credentials
   * 
   * @param userId User ID (case insensitive)
   * @param password Password (case insensitive)
   * @returns Observable<LoginResponse>
   */
  login(userId: string, password: string): Observable<LoginResponse> {
    const request: LoginRequest = {
      userId: userId.toUpperCase().trim(),
      password: password.toUpperCase().trim()
    };

    return this.http.post<LoginResponse>(`${this.API_URL}/login`, request).pipe(
      tap(response => {
        // Store user session
        const userSession: UserSession = {
          userId: response.userId,
          firstName: response.firstName,
          lastName: response.lastName,
          userType: response.userType,
          isAdmin: response.isAdmin
        };
        
        sessionStorage.setItem('currentUser', JSON.stringify(userSession));
        this.currentUserSubject.next(userSession);
      }),
      catchError(this.handleError)
    );
  }

  /**
   * Logout current user
   */
  logout(): Observable<void> {
    const userId = this.currentUserValue?.userId ?? '';
    
    return this.http.post<void>(`${this.API_URL}/logout`, { userId }).pipe(
      tap(() => {
        // Clear session
        sessionStorage.removeItem('currentUser');
        this.currentUserSubject.next(null);
      }),
      catchError(error => {
        // Even if logout fails on backend, clear local session
        sessionStorage.removeItem('currentUser');
        this.currentUserSubject.next(null);
        return throwError(() => error);
      })
    );
  }

  /**
   * Handle HTTP errors
   */
  private handleError(error: HttpErrorResponse) {
    let errorMessage = 'An error occurred';
    
    if (error.error instanceof ErrorEvent) {
      // Client-side error
      errorMessage = `Error: ${error.error.message}`;
    } else {
      // Server-side error
      if (error.status === 401) {
        errorMessage = error.error || 'Invalid user ID or password';
      } else if (error.status === 0) {
        errorMessage = 'Unable to connect to server. Please check if the backend is running.';
      } else {
        errorMessage = error.error || `Server error: ${error.status}`;
      }
    }
    
    return throwError(() => new Error(errorMessage));
  }
}
