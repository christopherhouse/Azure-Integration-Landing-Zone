       IDENTIFICATION DIVISION.
       PROGRAM-ID. STORAGE.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Storage Account deployment module
      *> Creates Azure Storage Accounts using ARM REST API
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-AZURE-CONFIG.
           COPY AZURECONFIG.
       01  LS-AUTH-TOKEN.
           COPY HTTPCLIENT REPLACING AZURE-AUTH-TOKEN BY LS-AUTH-TOKEN.
       01  LS-RETURN-CODE              PIC 9(2).
       
       WORKING-STORAGE SECTION.
       01  WS-STORAGE-URL              PIC X(512).
       01  WS-STORAGE-NAME             PIC X(64).
       01  WS-API-VERSION              PIC X(16) VALUE '2023-01-01'.
       01  WS-COUNTER                  PIC 9(2).
       
       01  WS-JSON-TEMPLATE.
           05  FILLER                  PIC X(50) VALUE
               '{"location":"'.
           05  JSON-LOCATION           PIC X(32).
           05  FILLER                  PIC X(50) VALUE
               '","sku":{"name":"'.
           05  JSON-SKU-NAME           PIC X(16).
           05  FILLER                  PIC X(50) VALUE
               '"},"kind":"'.
           05  JSON-KIND               PIC X(16).
           05  FILLER                  PIC X(50) VALUE
               '","properties":{"accessTier":"'.
           05  JSON-ACCESS-TIER        PIC X(8).
           05  FILLER                  PIC X(50) VALUE
               '","minimumTlsVersion":"TLS1_2",'.
           05  FILLER                  PIC X(50) VALUE
               '"allowBlobPublicAccess":false,'.
           05  FILLER                  PIC X(50) VALUE
               '"publicNetworkAccess":"Disabled"}}'.
       
       01  WS-REQUEST-JSON             PIC X(1024).
       
       PROCEDURE DIVISION USING LS-AZURE-CONFIG
                               LS-AUTH-TOKEN  
                               LS-RETURN-CODE.
       
       MAIN-PROCEDURE.
           DISPLAY "Storage Account deployment module starting..."
           MOVE ZERO TO LS-RETURN-CODE
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > STORAGE-COUNT
               PERFORM DEPLOY-STORAGE-ACCOUNT
           END-PERFORM
           
           GOBACK.
       
       DEPLOY-STORAGE-ACCOUNT.
           DISPLAY "Deploying storage account " WS-COUNTER "..."
           
      *> Generate storage account name using naming convention
           PERFORM GENERATE-STORAGE-NAME
           
      *> Build ARM API URL
           PERFORM BUILD-STORAGE-URL
           
      *> Prepare JSON payload
           PERFORM PREPARE-JSON-PAYLOAD
           
      *> Make ARM API call
           PERFORM MAKE-API-CALL
           
           IF HTTP-STATUS-CODE = 200 OR HTTP-STATUS-CODE = 201
               DISPLAY "Storage account created successfully: " WS-STORAGE-NAME
           ELSE
               DISPLAY "Failed to create storage account. Status: " 
                       HTTP-STATUS-CODE
               MOVE 1 TO LS-RETURN-CODE
           END-IF.
       
       GENERATE-STORAGE-NAME.
      *> Create unique storage account name using prefix + suffix + environment
           STRING 'sa'
                  SA-NAME-PREFIX(WS-COUNTER)
                  ENVIRONMENT
                  SUFFIX
               DELIMITED BY SPACE INTO WS-STORAGE-NAME
           END-STRING
           
      *> Convert to lowercase (storage account names must be lowercase)
           INSPECT WS-STORAGE-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'
           
      *> Remove any special characters (only alphanumeric allowed)
           INSPECT WS-STORAGE-NAME REPLACING ALL '-' BY SPACE
           INSPECT WS-STORAGE-NAME REPLACING ALL '_' BY SPACE
           MOVE FUNCTION SUBSTITUTE(WS-STORAGE-NAME, ' ', '') 
               TO WS-STORAGE-NAME.
       
       BUILD-STORAGE-URL.
           STRING 'https://management.azure.com/subscriptions/'
                  SUBSCRIPTION-ID
                  '/resourceGroups/'
                  RG-NAME
                  '/providers/Microsoft.Storage/storageAccounts/'
                  WS-STORAGE-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO WS-STORAGE-URL
           END-STRING.
       
       PREPARE-JSON-PAYLOAD.
      *> Fill in the JSON template with actual values
           MOVE RG-LOCATION TO JSON-LOCATION
           MOVE SA-SKU-NAME(WS-COUNTER) TO JSON-SKU-NAME
           MOVE SA-ACCOUNT-KIND(WS-COUNTER) TO JSON-KIND
           MOVE SA-ACCESS-TIER(WS-COUNTER) TO JSON-ACCESS-TIER
           
      *> Convert template to actual JSON string
           STRING WS-JSON-TEMPLATE DELIMITED BY LOW-VALUE
               INTO WS-REQUEST-JSON
           END-STRING.
       
       MAKE-API-CALL.
      *> Set up HTTP request for ARM API
           MOVE 'PUT' TO HTTP-METHOD
           MOVE WS-STORAGE-URL TO HTTP-URL
           MOVE 'application/json' TO CONTENT-TYPE
           
      *> Set authorization header with bearer token
           STRING 'Bearer ' ACCESS-TOKEN(LS-AUTH-TOKEN)
               DELIMITED BY SIZE INTO AUTHORIZATION
           END-STRING
           
           MOVE WS-REQUEST-JSON TO HTTP-BODY
           MOVE FUNCTION LENGTH(WS-REQUEST-JSON) TO CONTENT-LENGTH
           
      *> Make the HTTP call (simulated for demonstration)
           PERFORM SIMULATE-HTTP-CALL.
       
       SIMULATE-HTTP-CALL.
      *> In a real implementation, this would call external HTTP library
      *> For demonstration, we simulate a successful creation
           MOVE 201 TO HTTP-STATUS-CODE
           MOVE 'Created' TO HTTP-STATUS-TEXT
           
           DISPLAY "API Call simulated:"
           DISPLAY "  Method: " HTTP-METHOD
           DISPLAY "  URL: " WS-STORAGE-URL
           DISPLAY "  Body: " WS-REQUEST-JSON(1:200) "...".
       
       END PROGRAM STORAGE.