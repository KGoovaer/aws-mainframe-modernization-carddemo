-- Sample users for CardDemo POC
-- Maps to COBOL USRSEC file data

-- Administrator user
INSERT INTO users (user_id, password, user_type, first_name, last_name, created_at) 
VALUES ('ADMIN01', 'ADMIN01', 'A', 'System', 'Administrator', CURRENT_TIMESTAMP);

-- Regular users
INSERT INTO users (user_id, password, user_type, first_name, last_name, created_at) 
VALUES ('USER01', 'USER01', 'U', 'John', 'Doe', CURRENT_TIMESTAMP);

INSERT INTO users (user_id, password, user_type, first_name, last_name, created_at) 
VALUES ('USER02', 'USER02', 'U', 'Jane', 'Smith', CURRENT_TIMESTAMP);

INSERT INTO users (user_id, password, user_type, first_name, last_name, created_at) 
VALUES ('CSR001', 'CSR001', 'U', 'Alice', 'Johnson', CURRENT_TIMESTAMP);

INSERT INTO users (user_id, password, user_type, first_name, last_name, created_at) 
VALUES ('CSR002', 'CSR002', 'U', 'Bob', 'Williams', CURRENT_TIMESTAMP);
