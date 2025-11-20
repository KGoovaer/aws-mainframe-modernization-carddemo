# UC-008: Export Account Data

## Overview
**Actor**: System (Automated Batch Process), Data Analyst  
**Goal**: Export account master data in multiple formats for reporting, analysis, and integration with downstream systems  
**Frequency**: Daily or on-demand  
**Priority**: Medium - Supports reporting and data warehouse integration  

## Preconditions
- Account master file is accessible
- Output files or storage locations are available
- Date formatting utility is available
- User has authorization to export account data (if on-demand)

## Main Success Scenario
1. System initiates account export batch job (scheduled or on-demand)
2. System opens account master file for sequential reading
3. System opens three output files:
   - Standard sequential file
   - Array-format file
   - Variable-length record file
4. System reads account master file sequentially from beginning
5. For each account record:
   - System formats dates using date conversion utility
   - System writes record to sequential output file
   - System writes record to array-format output file
   - System writes record to variable-length output file
6. System continues reading and writing until end of file
7. System closes all files
8. System reports total records exported
9. System completes successfully

## Alternative Flows

### If user requests specific output format only
- System opens only requested output file
- System reads and writes records to single output
- System completes with single file produced

### If user specifies date range filter
- System reads all records but filters by account open date
- System exports only accounts opened within date range
- System reports total records read and total records exported

### If user requests specific account subset
- System accepts selection criteria (account status, group ID, etc.)
- System filters records during processing
- System exports only matching accounts

## Exception Flows

### If account master file cannot be opened
- System displays/logs error message
- System aborts export job
- System alerts operations staff
- User/scheduler must investigate and retry

### If output file cannot be created or written
- System logs error with file name and system response code
- System attempts to close open files gracefully
- System aborts export job
- Partial output files should be deleted or marked as incomplete

### If date formatting utility fails
- System logs error with account number and date value
- System may skip problematic record or use unformatted date
- System continues processing remaining accounts
- System includes skipped records in exception report

### If disk space exhausted during write
- System receives file write error
- System closes all files
- System logs error and available disk space
- System aborts job
- Operations must free space and restart export

## Business Rules Applied
- BR-002: Account Management (FR-002.5: Account Data Export)
- All account fields must be exported without data loss
- Date fields must be properly formatted for downstream consumption
- Export must preserve data integrity and accuracy

## Acceptance Criteria
- [ ] System exports all account records from master file
- [ ] System produces three distinct output formats successfully
- [ ] System formats dates consistently using standard utility
- [ ] System handles large datasets (500K+ accounts) efficiently
- [ ] System reports accurate record counts
- [ ] Output files are readable by downstream systems
- [ ] System completes export within defined time window
- [ ] System handles file I/O errors gracefully without data corruption
- [ ] Export data matches source data (verified by sampling)
- [ ] System creates audit log of export execution

## Source
**COBOL Program**: CBACT01C  
**Business Requirement**: BR-002 (FR-002.5: Account Data Export)
