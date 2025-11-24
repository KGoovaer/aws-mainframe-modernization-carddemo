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
