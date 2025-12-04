/**
 * Login request DTO
 * Maps to backend LoginRequest record
 */
export interface LoginRequest {
  userId: string;
  password: string;
}

/**
 * Login response DTO
 * Maps to backend LoginResponse record
 */
export interface LoginResponse {
  userId: string;
  userType: string;
  firstName: string;
  lastName: string;
  isAdmin: boolean;
  message: string;
}

/**
 * Current user session data
 */
export interface UserSession {
  userId: string;
  firstName: string;
  lastName: string;
  userType: string;
  isAdmin: boolean;
}
