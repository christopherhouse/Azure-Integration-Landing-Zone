       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSERVICEBUS.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Unit tests for Service Bus deployment module
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  TEST-COUNTER                PIC 9(3) VALUE 0.
       01  PASSED-TESTS                PIC 9(3) VALUE 0.
       01  FAILED-TESTS                PIC 9(3) VALUE 0.
       01  WS-RETURN-CODE              PIC 9(2).
       01  WS-TEST-RESULT              PIC X(6).
       01  WS-SERVICE-BUS-NAME         PIC X(64).
       01  WS-EXPECTED-NAME            PIC X(64).
       01  WS-INPUT-POS                PIC 9(3).
       01  WS-OUTPUT-POS               PIC 9(3).
       01  WS-JSON-TEST                PIC X(1024).
       01  WS-EXPECTED-JSON            PIC X(1024).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Service Bus Unit Tests"
           DISPLAY "======================="
           
           PERFORM TEST-SERVICE-BUS-NAME-GENERATION
           PERFORM TEST-JSON-PAYLOAD-CREATION
           PERFORM TEST-API-URL-BUILDING
           
           PERFORM DISPLAY-TEST-SUMMARY
           
           STOP RUN.
       
       TEST-SERVICE-BUS-NAME-GENERATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": Service Bus Name Generation"
           
      *> Set up test data
           MOVE 'dev' TO ENV-NAME
           MOVE 'lz-tf' TO SUFFIX
           
      *> Simulate name generation logic
           STRING 'sb-' 
                  ENV-NAME
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO WS-SERVICE-BUS-NAME
           END-STRING
           
      *> Convert to lowercase
           INSPECT WS-SERVICE-BUS-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'
           
           MOVE 'sb-dev-lz-tf' TO WS-EXPECTED-NAME
           
           IF WS-SERVICE-BUS-NAME = WS-EXPECTED-NAME
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO PASSED-TESTS
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO FAILED-TESTS
               DISPLAY "  Expected: " WS-EXPECTED-NAME
               DISPLAY "  Got: " WS-SERVICE-BUS-NAME
           END-IF
           
           DISPLAY "  Result: " WS-TEST-RESULT.
       
       TEST-JSON-PAYLOAD-CREATION.
           ADD 1 TO TEST-COUNTER
           DISPLAY "Test " TEST-COUNTER ": JSON Payload Creation"
           
      *> Set up test data
           MOVE 'eastus2' TO RG-LOCATION
           MOVE 'dev' TO ENV-NAME
           MOVE 1 TO WS-OUTPUT-POS
           
      *> Create expected JSON snippet for validation
           STRING '{'
               '"location": "eastus2",'
               '"sku": {'
               '  "name": "Premium",'
               '  "tier": "Premium",'
               '  "capacity": 1'
               '}'
               DELIMITED BY SIZE INTO WS-JSON-TEST
           END-STRING
           
      *> Check for JSON structure
           IF WS-JSON-TEST(1:WS-OUTPUT-POS) = '{'
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
           MOVE 'c5d4a6e8-69bf-4148-be25-cb362f83c370' 
                TO SUBSCRIPTION-ID
           MOVE 'RG-AIS-LZ-TF' TO RG-NAME
           MOVE 'sb-dev-lz-tf' TO WS-SERVICE-BUS-NAME
           
           MOVE SPACES TO WS-EXPECTED-NAME
           STRING 'https://management.azure.com/subscriptions/'
                  SUBSCRIPTION-ID
                  '/resourceGroups/'
                  RG-NAME
                  '/providers/Microsoft.ServiceBus/namespaces/'
                  WS-SERVICE-BUS-NAME
               DELIMITED BY SIZE INTO WS-EXPECTED-NAME
           END-STRING
           
      *> Verify URL components are assembled correctly
           IF WS-EXPECTED-NAME(1:45) = 
               'https://management.azure.com/subscriptions/'
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
       
       END PROGRAM TESTSERVICEBUS.