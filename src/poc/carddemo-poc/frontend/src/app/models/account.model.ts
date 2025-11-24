/**
 * Account Management Models
 * Maps to backend DTOs for FEAT-POC-002
 */

/**
 * Customer DTO
 * Maps to backend CustomerDto record
 */
export interface Customer {
  customerId: string;
  firstName: string;
  middleName: string | null;
  lastName: string;
  addressLine1: string;
  addressLine2: string | null;
  addressLine3: string | null;
  stateCode: string;
  countryCode: string;
  zip: string;
  phoneNumber1: string;
  phoneNumber2: string | null;
  ssn: string;
  governmentId: string | null;
  dateOfBirth: string;
  eftAccountId: string;
  primaryCardHolder: string;
  ficoScore: number;
}

/**
 * Account Details DTO
 * Maps to backend AccountDetailsDto record
 */
export interface AccountDetails {
  accountId: string;
  customerId: string;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string | null;
  reissueDate: string | null;
  currentCycleCredit: number;
  currentCycleDebit: number;
  addressZip: string;
  groupId: string | null;
  customer: Customer;
}

/**
 * Account Update Request
 * Subset of fields that can be updated
 */
export interface AccountUpdate {
  creditLimit?: number;
  cashCreditLimit?: number;
  activeStatus?: string;
  expirationDate?: string;
  reissueDate?: string;
  groupId?: string;
}

/**
 * Customer Update Request
 * Subset of customer fields that can be updated
 */
export interface CustomerUpdate {
  addressLine1?: string;
  addressLine2?: string;
  addressLine3?: string;
  stateCode?: string;
  countryCode?: string;
  zip?: string;
  phoneNumber1?: string;
  phoneNumber2?: string;
  ficoScore?: number;
}

/**
 * Combined Update Request
 * Maps to backend UpdateAccountAndCustomerRequest
 */
export interface UpdateAccountRequest {
  accountUpdate?: AccountUpdate;
  customerUpdate?: CustomerUpdate;
}
