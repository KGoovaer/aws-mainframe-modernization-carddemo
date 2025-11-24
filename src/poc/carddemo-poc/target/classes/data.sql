-- Sample users for CardDemo POC
-- Maps to COBOL USRSEC file data
-- Using MERGE to avoid duplicate key errors on restart

-- Administrator user
MERGE INTO users (user_id, password, user_type, first_name, last_name, created_at) 
KEY(user_id)
VALUES ('ADMIN01', 'ADMIN01', 'A', 'System', 'Administrator', CURRENT_TIMESTAMP);

-- Regular users
MERGE INTO users (user_id, password, user_type, first_name, last_name, created_at) 
KEY(user_id)
VALUES ('USER01', 'USER01', 'U', 'John', 'Doe', CURRENT_TIMESTAMP);

MERGE INTO users (user_id, password, user_type, first_name, last_name, created_at) 
KEY(user_id)
VALUES ('USER02', 'USER02', 'U', 'Jane', 'Smith', CURRENT_TIMESTAMP);

MERGE INTO users (user_id, password, user_type, first_name, last_name, created_at) 
KEY(user_id)
VALUES ('CSR001', 'CSR001', 'U', 'Alice', 'Johnson', CURRENT_TIMESTAMP);

MERGE INTO users (user_id, password, user_type, first_name, last_name, created_at) 
KEY(user_id)
VALUES ('CSR002', 'CSR002', 'U', 'Bob', 'Williams', CURRENT_TIMESTAMP);

-- Sample customers for Account Management POC
-- Maps to COBOL CVCUS01Y copybook

MERGE INTO customers (customer_id, first_name, middle_name, last_name, 
    address_line_1, address_line_2, address_line_3, state_code, country_code, zip,
    phone_number_1, phone_number_2, ssn, government_id, date_of_birth,
    eft_account_id, primary_card_holder, fico_score)
KEY(customer_id)
VALUES ('000000001', 'John', 'A', 'Doe', 
    '123 Main St', '', '', 'IL', 'USA', '62701',
    '555-1234', '', '123456789', 'DL12345', '1980-05-15',
    'EFT001', 'Y', 720);

MERGE INTO customers (customer_id, first_name, middle_name, last_name, 
    address_line_1, address_line_2, address_line_3, state_code, country_code, zip,
    phone_number_1, phone_number_2, ssn, government_id, date_of_birth,
    eft_account_id, primary_card_holder, fico_score)
KEY(customer_id)
VALUES ('000000002', 'Jane', 'M', 'Smith', 
    '456 Oak Ave', 'Apt 2B', '', 'CA', 'USA', '90210',
    '555-5678', '555-9999', '987654321', 'DL67890', '1985-08-22',
    'EFT002', 'Y', 680);

MERGE INTO customers (customer_id, first_name, middle_name, last_name, 
    address_line_1, address_line_2, address_line_3, state_code, country_code, zip,
    phone_number_1, phone_number_2, ssn, government_id, date_of_birth,
    eft_account_id, primary_card_holder, fico_score)
KEY(customer_id)
VALUES ('000000003', 'Robert', 'B', 'Johnson', 
    '789 Elm St', '', '', 'NY', 'USA', '10001',
    '555-2468', '', '456789123', 'DL24680', '1975-12-10',
    'EFT003', 'Y', 750);

-- Sample accounts for Account Management POC
-- Maps to COBOL CVACT01Y copybook

MERGE INTO accounts (account_id, customer_id, active_status, 
    current_balance, credit_limit, cash_credit_limit,
    open_date, expiration_date, reissue_date,
    current_cycle_credit, current_cycle_debit,
    address_zip, group_id)
KEY(account_id)
VALUES ('00000000001', '000000001', 'Y', 
    1250.50, 5000.00, 1000.00,
    '2024-01-15', '2027-01-15', NULL,
    0.00, 0.00,
    '62701', 'STANDARD');

MERGE INTO accounts (account_id, customer_id, active_status, 
    current_balance, credit_limit, cash_credit_limit,
    open_date, expiration_date, reissue_date,
    current_cycle_credit, current_cycle_debit,
    address_zip, group_id)
KEY(account_id)
VALUES ('00000000002', '000000002', 'Y', 
    3500.75, 10000.00, 2000.00,
    '2023-06-20', '2026-06-20', '2024-06-20',
    500.00, 250.00,
    '90210', 'PREMIUM');

MERGE INTO accounts (account_id, customer_id, active_status, 
    current_balance, credit_limit, cash_credit_limit,
    open_date, expiration_date, reissue_date,
    current_cycle_credit, current_cycle_debit,
    address_zip, group_id)
KEY(account_id)
VALUES ('00000000003', '000000003', 'N', 
    150.00, 2500.00, 500.00,
    '2022-03-10', '2025-03-10', NULL,
    0.00, 0.00,
    '10001', 'STANDARD');
