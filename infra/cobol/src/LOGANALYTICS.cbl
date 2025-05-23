       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGANALYTICS.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Log Analytics Workspace deployment module
      *> Creates Azure Log Analytics Workspace using ARM REST API
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-AZURE-CONFIG.
           COPY AZURECONFIG.
       01  LS-AUTH-TOKEN.
           COPY HTTPCLIENT REPLACING AZURE-AUTH-TOKEN BY LS-AUTH-TOKEN.
       01  LS-RETURN-CODE              PIC 9(2).
       
       WORKING-STORAGE SECTION.
       01  WS-LA-URL                   PIC X(512).
       01  WS-LA-NAME                  PIC X(64).
       01  WS-API-VERSION              PIC X(16) VALUE '2023-09-01'.
       
       01  WS-JSON-TEMPLATE.
           05  FILLER                  PIC X(30) VALUE
               '{"location":"'.
           05  JSON-LOCATION           PIC X(32).
           05  FILLER                  PIC X(50) VALUE
               '","properties":{"sku":{"name":"PerGB2018"},'.
           05  FILLER                  PIC X(50) VALUE
               '"retentionInDays":30,'.
           05  FILLER                  PIC X(50) VALUE
               '"features":{"enableLogAccessUsingOnlyResourcePermissions":true},'.
           05  FILLER                  PIC X(50) VALUE
               '"workspaceCapping":{"dailyQuotaGb":-1},'.
           05  FILLER                  PIC X(50) VALUE
               '"publicNetworkAccessForIngestion":"Enabled",'.
           05  FILLER                  PIC X(50) VALUE
               '"publicNetworkAccessForQuery":"Enabled"}}'. 
       
       01  WS-REQUEST-JSON             PIC X(1024).
       
       PROCEDURE DIVISION USING LS-AZURE-CONFIG
                               LS-AUTH-TOKEN  
                               LS-RETURN-CODE.
       
       MAIN-PROCEDURE.
           DISPLAY "Log Analytics Workspace deployment module starting..."
           MOVE ZERO TO LS-RETURN-CODE
           
           PERFORM DEPLOY-LOG-ANALYTICS
           
           GOBACK.
       
       DEPLOY-LOG-ANALYTICS.
           DISPLAY "Deploying Azure Log Analytics Workspace..."
           
      *> Generate Log Analytics name using naming convention
           PERFORM GENERATE-LA-NAME
           
      *> Build ARM API URL
           PERFORM BUILD-LA-URL
           
      *> Prepare JSON payload
           PERFORM PREPARE-JSON-PAYLOAD
           
      *> Make ARM API call
           PERFORM MAKE-API-CALL
           
           IF HTTP-STATUS-CODE = 200 OR HTTP-STATUS-CODE = 201
               DISPLAY "Log Analytics Workspace created successfully: " 
                       WS-LA-NAME
           ELSE
               DISPLAY "Failed to create Log Analytics Workspace. Status: " 
                       HTTP-STATUS-CODE
               MOVE 1 TO LS-RETURN-CODE
           END-IF.
       
       GENERATE-LA-NAME.
      *> Create Log Analytics name using naming convention
           STRING 'law-'
                  ENVIRONMENT
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO WS-LA-NAME
           END-STRING
           
      *> Convert to lowercase
           INSPECT WS-LA-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'.
       
       BUILD-LA-URL.
           STRING 'https://management.azure.com/subscriptions/'
                  SUBSCRIPTION-ID
                  '/resourceGroups/'
                  RG-NAME
                  '/providers/Microsoft.OperationalInsights/workspaces/'
                  WS-LA-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO WS-LA-URL
           END-STRING.
       
       PREPARE-JSON-PAYLOAD.
      *> Fill in the JSON template with actual values
           MOVE RG-LOCATION TO JSON-LOCATION
           
      *> Convert template to actual JSON string
           STRING WS-JSON-TEMPLATE DELIMITED BY LOW-VALUE
               INTO WS-REQUEST-JSON
           END-STRING.
       
       MAKE-API-CALL.
      *> Set up HTTP request for ARM API
           MOVE 'PUT' TO HTTP-METHOD
           MOVE WS-LA-URL TO HTTP-URL
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
           DISPLAY "  URL: " WS-LA-URL
           DISPLAY "  Body: " WS-REQUEST-JSON(1:200) "...".
       
       END PROGRAM LOGANALYTICS.