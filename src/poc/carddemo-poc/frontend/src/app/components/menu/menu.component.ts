import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { AuthService } from '../../services/auth.service';
import { UserSession } from '../../models/auth.model';

/**
 * Main Menu Component
 * 
 * Implements User Story US-008: Role-Based Login Routing
 * Maps to COBOL program COMEN01C (Main Menu Screen)
 * 
 * Features:
 * - Display user information
 * - Show available menu options based on user role
 * - Navigate to different functions
 * - Logout functionality (US-005)
 */
@Component({
  selector: 'app-menu',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.css']
})
export class MenuComponent implements OnInit {
  currentUser: UserSession | null = null;

  // Menu items - will be filtered based on user role
  menuItems = [
    { code: '1', label: 'Account View/Update', route: '/accounts', adminOnly: false },
    { code: '2', label: 'Card View/Update', route: '/cards', adminOnly: false },
    { code: '3', label: 'Transaction View', route: '/transactions', adminOnly: false },
    { code: '4', label: 'User Administration', route: '/users', adminOnly: true },
    { code: '5', label: 'Bill Payment', route: '/bill-payment', adminOnly: false },
    { code: '6', label: 'Account Reports', route: '/reports', adminOnly: false }
  ];

  constructor(
    private authService: AuthService,
    private router: Router
  ) {}

  ngOnInit(): void {
    // Get current user
    this.authService.currentUser$.subscribe(user => {
      this.currentUser = user;
      
      // Redirect to login if not authenticated
      if (!user) {
        this.router.navigate(['/login']);
      }
    });
  }

  /**
   * Get filtered menu items based on user role
   */
  get availableMenuItems() {
    return this.menuItems.filter(item => {
      // Show all non-admin items, or admin items only to admins
      return !item.adminOnly || this.currentUser?.isAdmin;
    });
  }

  /**
   * Navigate to selected menu option
   */
  selectMenuItem(item: any): void {
    // Navigate to implemented screens, show alert for others
    if (item.route === '/accounts') {
      this.router.navigate([item.route]);
    } else {
      // For other POC screens not yet implemented
      alert(`Navigation to ${item.label} - Coming soon in POC`);
    }
  }

  /**
   * Handle logout
   * US-005: User Logout
   * COBOL: F3 key handling in main menu
   */
  onLogout(): void {
    this.authService.logout().subscribe({
      next: () => {
        this.router.navigate(['/login']);
      },
      error: (error) => {
        console.error('Logout error:', error);
        // Still navigate to login even if logout fails
        this.router.navigate(['/login']);
      }
    });
  }

  /**
   * Get user display name
   */
  get userDisplayName(): string {
    if (!this.currentUser) return '';
    return `${this.currentUser.firstName} ${this.currentUser.lastName}`;
  }

  /**
   * Get user type label
   */
  get userTypeLabel(): string {
    return this.currentUser?.isAdmin ? 'Administrator' : 'Regular User';
  }
}
