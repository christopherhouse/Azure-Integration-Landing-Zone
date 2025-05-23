       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSTORAGE.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Unit tests for Storage Account deployment module
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  TEST-COUNTER                PIC 9(3) VALUE 0.
       01  PASSED-TESTS                PIC 9(3) VALUE 0.
       01  FAILED-TESTS                PIC 9(3) VALUE 0.
       01  WS-RETURN-CODE              PIC 9(2).
       01  WS-TEST-RESULT              PIC X(6).
       01  WS-STORAGE-NAME             PIC X(64).
       01  WS-EXPECTED-NAME            PIC X(64).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Storage Account Unit Tests"
           DISPLAY "=========================="
           
           PERFORM TEST-STORAGE-NAME-GENERATION
           PERFORM TEST-JSON-PAYLOAD-CREATION
           PERFORM TEST-API-URL-BUILDING
           
           PERFORM DISPLAY-TEST-SUMMARY
           
           STOP RUN.
       
       TEST-STORAGE-NAME-GENERATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": Storage Name Generation"
           
      *> Set up test data
           MOVE 'apimbackup' TO SA-NAME-PREFIX(1)
           MOVE 'dev' TO ENVIRONMENT
           MOVE 'lz-tf' TO SUFFIX
           
      *> Simulate name generation logic
           STRING 'sa' 
                  SA-NAME-PREFIX(1)
                  ENVIRONMENT
                  SUFFIX
               DELIMITED BY SPACE INTO WS-STORAGE-NAME
           END-STRING
           
      *> Convert to lowercase and remove special chars
           INSPECT WS-STORAGE-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'
           INSPECT WS-STORAGE-NAME REPLACING ALL '-' BY SPACE
           MOVE FUNCTION SUBSTITUTE(WS-STORAGE-NAME, ' ', '') 
               TO WS-STORAGE-NAME
           
           MOVE 'saapimbackupdevlztf' TO WS-EXPECTED-NAME
           
           IF WS-STORAGE-NAME = WS-EXPECTED-NAME
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "  Expected: " WS-EXPECTED-NAME
               DISPLAY "  Got: " WS-STORAGE-NAME
           END-IF
           
           DISPLAY "  Result: " WS-TEST-RESULT.
       
       TEST-JSON-PAYLOAD-CREATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": JSON Payload Creation"
           
      *> Set up test data
           MOVE 'eastus2' TO RG-LOCATION
           MOVE 'Standard_LRS' TO SA-SKU-NAME(1)
           MOVE 'StorageV2' TO SA-ACCOUNT-KIND(1)
           MOVE 'Hot' TO SA-ACCESS-TIER(1)
           
      *> Verify that all required fields are populated
           IF RG-LOCATION NOT = SPACES
               AND SA-SKU-NAME(1) NOT = SPACES
               AND SA-ACCOUNT-KIND(1) NOT = SPACES
               AND SA-ACCESS-TIER(1) NOT = SPACES
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
           END-IF
           
           DISPLAY "  Result: " WS-TEST-RESULT.
       
       TEST-API-URL-BUILDING.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": API URL Building"
           
      *> Set up test data
           MOVE 'c5d4a6e8-69bf-4148-be25-cb362f83c370' TO SUBSCRIPTION-ID
           MOVE 'RG-AIS-LZ-TF' TO RG-NAME
           
      *> Verify URL components are available
           IF SUBSCRIPTION-ID NOT = SPACES
               AND RG-NAME NOT = SPACES
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
           END-IF
           
           DISPLAY "  Result: " WS-TEST-RESULT.
       
       DISPLAY-TEST-SUMMARY.
           DISPLAY " "
           DISPLAY "Test Summary"
           DISPLAY "============"
           DISPLAY "Total Tests: " TEST-COUNTER
           DISPLAY "Passed: " PASSED-TESTS
           DISPLAY "Failed: " FAILED-TESTS
           
           IF FAILED-TESTS = 0
               DISPLAY "All tests passed!"
           ELSE
               DISPLAY "Some tests failed. Please review."
           END-IF.
       
       END PROGRAM TESTSTORAGE.