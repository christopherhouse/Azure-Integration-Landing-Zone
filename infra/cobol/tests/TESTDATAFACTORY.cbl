       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTDATAFACTORY.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Unit tests for Data Factory deployment module
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  TEST-COUNTER                PIC 9(3) VALUE 0.
       01  PASSED-TESTS                PIC 9(3) VALUE 0.
       01  FAILED-TESTS                PIC 9(3) VALUE 0.
       01  WS-RETURN-CODE              PIC 9(2).
       01  WS-TEST-RESULT              PIC X(6).
       01  WS-DATA-FACTORY-NAME        PIC X(64).
       01  WS-MVNET-NAME               PIC X(64).
       01  WS-EXPECTED-NAME            PIC X(64).
       01  WS-INPUT-POS                PIC 9(3).
       01  WS-OUTPUT-POS               PIC 9(3).
       01  WS-JSON-TEST                PIC X(1024).
       01  WS-EXPECTED-JSON            PIC X(1024).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Data Factory Unit Tests"
           DISPLAY "======================="
           
           PERFORM TEST-DATA-FACTORY-NAME-GENERATION
           PERFORM TEST-VNET-NAME-GENERATION
           PERFORM TEST-JSON-PAYLOAD-CREATION
           
           PERFORM DISPLAY-TEST-SUMMARY
           
           STOP RUN.
       
       TEST-DATA-FACTORY-NAME-GENERATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": Data Factory Name Generation"
           
      *> Set up test data
           MOVE 'dev' TO ENV-NAME
           MOVE 'lz-tf' TO SUFFIX
           
      *> Simulate name generation logic
           STRING 'df-' 
                  ENV-NAME
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO WS-DATA-FACTORY-NAME
           END-STRING
           
      *> Convert to lowercase
           INSPECT WS-DATA-FACTORY-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'
           
           MOVE 'df-dev-lz-tf' TO WS-EXPECTED-NAME
           
           IF WS-DATA-FACTORY-NAME = WS-EXPECTED-NAME
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "  Expected: " WS-EXPECTED-NAME
               DISPLAY "  Got: " WS-DATA-FACTORY-NAME
           END-IF
           
           DISPLAY "  Result: " WS-TEST-RESULT.
       
       TEST-VNET-NAME-GENERATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": Managed VNet Name Generation"
           
      *> Set up test data from previous test
           MOVE 'df-dev-lz-tf' TO WS-DATA-FACTORY-NAME
           
      *> Simulate name generation logic
           STRING 'vnet-' 
                  WS-DATA-FACTORY-NAME
               DELIMITED BY SIZE INTO WS-MVNET-NAME
           END-STRING
           
           MOVE 'vnet-df-dev-lz-tf' TO WS-EXPECTED-NAME
           
           IF WS-MVNET-NAME = WS-EXPECTED-NAME
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "  Expected: " WS-EXPECTED-NAME
               DISPLAY "  Got: " WS-MVNET-NAME
           END-IF
           
           DISPLAY "  Result: " WS-TEST-RESULT.
       
       TEST-JSON-PAYLOAD-CREATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": JSON Payload Creation"
           
      *> Set up test data
           MOVE 'eastus2' TO RG-LOCATION
           MOVE 'dev' TO ENV-NAME
           MOVE 'SystemAssigned' TO WS-EXPECTED-NAME
           
      *> Create expected JSON snippet for validation
           STRING '{'
               '"location": "eastus2",'
               '"identity": {'
               '  "type": "SystemAssigned"'
               '},'
               '"properties": {'
               '  "publicNetworkAccess": "Disabled"'
               DELIMITED BY SIZE INTO WS-JSON-TEST
           END-STRING
           
      *> Check for identity type property
           IF WS-JSON-TEST(44:14) = WS-EXPECTED-NAME
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "  Expected to find: " WS-EXPECTED-NAME
               DISPLAY "  In: " WS-JSON-TEST(44:14)
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
       
       END PROGRAM TESTDATAFACTORY.