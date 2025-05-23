       IDENTIFICATION DIVISION.
       PROGRAM-ID. AZUREAUTH.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> This program handles Azure authentication using service principal
      *> and obtains access tokens for ARM API calls
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONFIG-FILE ASSIGN TO "config/azure-config.conf"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CONFIG-FILE.
       01  CONFIG-RECORD               PIC X(120).
       
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  WS-CONFIG-LINE              PIC X(120).
       01  WS-KEY                      PIC X(32).
       01  WS-VALUE                    PIC X(88).
       01  WS-EQUAL-POS                PIC 9(3).
       01  WS-EOF-FLAG                 PIC X VALUE 'N'.
       01  WS-RETURN-CODE              PIC 9(2).
       
       01  AUTH-REQUEST-BODY.
           05  FILLER                  PIC X(15) VALUE 'grant_type=client'.
           05  FILLER                  PIC X(12) VALUE '_credentials'.
           05  FILLER                  PIC X(11) VALUE '&client_id='.
           05  CLIENT-ID-VALUE         PIC X(36).
           05  FILLER                  PIC X(16) VALUE '&client_secret='.
           05  CLIENT-SECRET-VALUE     PIC X(256).
           05  FILLER                  PIC X(10) VALUE '&resource='.
           05  FILLER                  PIC X(41) VALUE 
               'https%3A%2F%2Fmanagement.azure.com%2F'.
       
       01  WS-AUTH-URL                 PIC X(512).
       01  WS-CURRENT-TIME             PIC 9(14).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Azure Authentication Module Starting..."
           
           PERFORM LOAD-CONFIGURATION
           PERFORM AUTHENTICATE-WITH-AZURE
           PERFORM CHECK-TOKEN-VALIDITY
           
           IF WS-RETURN-CODE = 0
               DISPLAY "Authentication successful"
           ELSE
               DISPLAY "Authentication failed with code: " WS-RETURN-CODE
           END-IF
           
           STOP RUN.
       
       LOAD-CONFIGURATION.
           DISPLAY "Loading configuration from file..."
           OPEN INPUT CONFIG-FILE
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CONFIG-FILE INTO WS-CONFIG-LINE
                   AT END MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END PERFORM PARSE-CONFIG-LINE
               END-READ
           END-PERFORM
           
           CLOSE CONFIG-FILE.
       
       PARSE-CONFIG-LINE.
           IF WS-CONFIG-LINE(1:1) NOT = '#' AND WS-CONFIG-LINE NOT = SPACES
               INSPECT WS-CONFIG-LINE TALLYING WS-EQUAL-POS FOR ALL '='
               IF WS-EQUAL-POS > 0
                   UNSTRING WS-CONFIG-LINE DELIMITED BY '='
                       INTO WS-KEY, WS-VALUE
                   END-UNSTRING
                   PERFORM PROCESS-CONFIG-VALUE
               END-IF
           END-IF.
       
       PROCESS-CONFIG-VALUE.
           EVALUATE WS-KEY
               WHEN 'SUBSCRIPTION_ID'
                   MOVE WS-VALUE TO SUBSCRIPTION-ID
               WHEN 'TENANT_ID'
                   MOVE WS-VALUE TO TENANT-ID
               WHEN 'CLIENT_ID'
                   MOVE WS-VALUE TO CLIENT-ID
               WHEN 'CLIENT_SECRET'
                   MOVE WS-VALUE TO CLIENT-SECRET
               WHEN 'RESOURCE_GROUP_NAME'
                   MOVE WS-VALUE TO RG-NAME
               WHEN 'LOCATION'
                   MOVE WS-VALUE TO RG-LOCATION
               WHEN 'ENVIRONMENT'
                   MOVE WS-VALUE TO ENVIRONMENT
               WHEN 'SUFFIX'
                   MOVE WS-VALUE TO SUFFIX
           END-EVALUATE.
       
       AUTHENTICATE-WITH-AZURE.
           DISPLAY "Authenticating with Azure Active Directory..."
           
      *> Build authentication URL
           STRING AUTH-ENDPOINT DELIMITED BY SPACE
                  TENANT-ID DELIMITED BY SPACE
                  '/oauth2/token' DELIMITED BY SIZE
               INTO WS-AUTH-URL
           END-STRING
           
      *> Prepare request body
           MOVE CLIENT-ID TO CLIENT-ID-VALUE
           MOVE CLIENT-SECRET TO CLIENT-SECRET-VALUE
           
      *> Set up HTTP request
           MOVE 'POST' TO HTTP-METHOD
           MOVE WS-AUTH-URL TO HTTP-URL
           MOVE 'application/x-www-form-urlencoded' TO CONTENT-TYPE
           MOVE AUTH-REQUEST-BODY TO HTTP-BODY
           MOVE FUNCTION LENGTH(AUTH-REQUEST-BODY) TO CONTENT-LENGTH
           
      *> Make HTTP call (this would be implemented using system calls
      *> or external HTTP client library in a real implementation)
           PERFORM MAKE-HTTP-REQUEST
           
           IF HTTP-STATUS-CODE = 200
               PERFORM PARSE-AUTH-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               DISPLAY "Authentication failed. Status: " HTTP-STATUS-CODE
               MOVE 1 TO WS-RETURN-CODE
           END-IF.
       
       MAKE-HTTP-REQUEST.
      *> In a real implementation, this would call external HTTP library
      *> For demonstration purposes, we'll simulate a successful response
           MOVE 200 TO HTTP-STATUS-CODE
           MOVE 'OK' TO HTTP-STATUS-TEXT
           STRING '{"access_token":"******'
                  'OiJSUzI1NiIs...","token_type":"Bearer",'
                  '"expires_in":3599}'
               DELIMITED BY SIZE INTO HTTP-RESPONSE-BODY
           END-STRING.
       
       PARSE-AUTH-RESPONSE.
      *> Simple JSON parsing for access token
      *> In a real implementation, this would use proper JSON parser
           MOVE 'Bearer' TO TOKEN-TYPE
           MOVE '******' TO ACCESS-TOKEN
           MOVE 3599 TO EXPIRES-IN
           
      *> Calculate expiry time
           ACCEPT WS-CURRENT-TIME FROM DATE YYYYMMDD
           COMPUTE TOKEN-EXPIRY-TIME = WS-CURRENT-TIME + EXPIRES-IN.
       
       CHECK-TOKEN-VALIDITY.
           ACCEPT WS-CURRENT-TIME FROM DATE YYYYMMDD
           IF WS-CURRENT-TIME < TOKEN-EXPIRY-TIME
               DISPLAY "Token is valid"
           ELSE
               DISPLAY "Token has expired, re-authentication needed"
               MOVE 2 TO WS-RETURN-CODE
           END-IF.
       
       END PROGRAM AZUREAUTH.