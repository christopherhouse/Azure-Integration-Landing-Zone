       IDENTIFICATION DIVISION.
       PROGRAM-ID. KEYVAULT.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Key Vault deployment module
      *> Creates Azure Key Vault using ARM REST API
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-AZURE-CONFIG.
           COPY AZURECONFIG.
       01  LS-AUTH-TOKEN.
           COPY HTTPCLIENT REPLACING AZURE-AUTH-TOKEN BY LS-AUTH-TOKEN.
       01  LS-RETURN-CODE              PIC 9(2).
       
       WORKING-STORAGE SECTION.
       01  WS-KV-URL                   PIC X(512).
       01  WS-KV-NAME                  PIC X(64).
       01  WS-API-VERSION              PIC X(16) VALUE '2023-07-01'.
       
       01  WS-JSON-TEMPLATE.
           05  FILLER                  PIC X(30) VALUE
               '{"location":"'.
           05  JSON-LOCATION           PIC X(32).
           05  FILLER                  PIC X(30) VALUE
               '","properties":{"tenantId":"'.
           05  JSON-TENANT-ID          PIC X(36).
           05  FILLER                  PIC X(50) VALUE
               '","sku":{"family":"A","name":"standard"},'.
           05  FILLER                  PIC X(50) VALUE
               '"enabledForDeployment":true,'.
           05  FILLER                  PIC X(50) VALUE
               '"enabledForTemplateDeployment":true,'.
           05  FILLER                  PIC X(50) VALUE
               '"enabledForDiskEncryption":true,'.
           05  FILLER                  PIC X(50) VALUE
               '"enableRbacAuthorization":true,'.
           05  FILLER                  PIC X(50) VALUE
               '"enableSoftDelete":true,'.
           05  FILLER                  PIC X(50) VALUE
               '"softDeleteRetentionInDays":'.
           05  JSON-SOFT-DELETE-DAYS   PIC X(3).
           05  FILLER                  PIC X(30) VALUE
               ',"enablePurgeProtection":'.
           05  JSON-PURGE-PROTECTION   PIC X(5).
           05  FILLER                  PIC X(50) VALUE
               ',"publicNetworkAccess":"Disabled"}}'. 
       
       01  WS-REQUEST-JSON             PIC X(1024).
       
       PROCEDURE DIVISION USING LS-AZURE-CONFIG
                               LS-AUTH-TOKEN  
                               LS-RETURN-CODE.
       
       MAIN-PROCEDURE.
           DISPLAY "Key Vault deployment module starting..."
           MOVE ZERO TO LS-RETURN-CODE
           
           PERFORM DEPLOY-KEY-VAULT
           
           GOBACK.
       
       DEPLOY-KEY-VAULT.
           DISPLAY "Deploying Azure Key Vault..."
           
      *> Generate Key Vault name using naming convention
           PERFORM GENERATE-KV-NAME
           
      *> Build ARM API URL
           PERFORM BUILD-KV-URL
           
      *> Prepare JSON payload
           PERFORM PREPARE-JSON-PAYLOAD
           
      *> Make ARM API call
           PERFORM MAKE-API-CALL
           
           IF HTTP-STATUS-CODE = 200 OR HTTP-STATUS-CODE = 201
               DISPLAY "Key Vault created successfully: " WS-KV-NAME
           ELSE
               DISPLAY "Failed to create Key Vault. Status: " 
                       HTTP-STATUS-CODE
               MOVE 1 TO LS-RETURN-CODE
           END-IF.
       
       GENERATE-KV-NAME.
      *> Create unique Key Vault name using naming convention
           STRING 'kv-'
                  ENVIRONMENT
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO WS-KV-NAME
           END-STRING
           
      *> Convert to lowercase (Key Vault names should be lowercase)
           INSPECT WS-KV-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'.
       
       BUILD-KV-URL.
           STRING 'https://management.azure.com/subscriptions/'
                  SUBSCRIPTION-ID
                  '/resourceGroups/'
                  RG-NAME
                  '/providers/Microsoft.KeyVault/vaults/'
                  WS-KV-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO WS-KV-URL
           END-STRING.
       
       PREPARE-JSON-PAYLOAD.
      *> Fill in the JSON template with actual values
           MOVE RG-LOCATION TO JSON-LOCATION
           MOVE TENANT-ID TO JSON-TENANT-ID
           MOVE KV-SOFT-DELETE-DAYS TO JSON-SOFT-DELETE-DAYS
           
      *> Set purge protection flag
           IF KV-PURGE-PROTECTION = 'Y'
               MOVE 'true' TO JSON-PURGE-PROTECTION
           ELSE
               MOVE 'false' TO JSON-PURGE-PROTECTION
           END-IF
           
      *> Convert template to actual JSON string
           STRING WS-JSON-TEMPLATE DELIMITED BY LOW-VALUE
               INTO WS-REQUEST-JSON
           END-STRING.
       
       MAKE-API-CALL.
      *> Set up HTTP request for ARM API
           MOVE 'PUT' TO HTTP-METHOD
           MOVE WS-KV-URL TO HTTP-URL
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
           DISPLAY "  URL: " WS-KV-URL
           DISPLAY "  Body: " WS-REQUEST-JSON(1:200) "...".
       
       END PROGRAM KEYVAULT.