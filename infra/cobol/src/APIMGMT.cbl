       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIMGMT.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> API Management deployment module
      *> Creates Azure API Management using ARM REST API
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-AZURE-CONFIG.
           COPY AZURECONFIG.
       01  LS-AUTH-TOKEN.
           COPY HTTPCLIENT REPLACING AZURE-AUTH-TOKEN BY LS-AUTH-TOKEN.
       01  LS-RETURN-CODE              PIC 9(2).
       
       WORKING-STORAGE SECTION.
       01  WS-APIM-URL                 PIC X(512).
       01  WS-APIM-NAME                PIC X(64).
       01  WS-API-VERSION              PIC X(16) VALUE '2023-05-01-preview'.
       
       01  WS-JSON-TEMPLATE.
           05  FILLER                  PIC X(30) VALUE
               '{"location":"'.
           05  JSON-LOCATION           PIC X(32).
           05  FILLER                  PIC X(30) VALUE
               '","sku":{"name":"'.
           05  JSON-SKU-NAME           PIC X(16).
           05  FILLER                  PIC X(30) VALUE
               '","capacity":'.
           05  JSON-SKU-CAPACITY       PIC X(2).
           05  FILLER                  PIC X(50) VALUE
               '},"properties":{"publisherName":"'.
           05  JSON-PUBLISHER-NAME     PIC X(64).
           05  FILLER                  PIC X(30) VALUE
               '","publisherEmail":"'.
           05  JSON-PUBLISHER-EMAIL    PIC X(128).
           05  FILLER                  PIC X(50) VALUE
               '","virtualNetworkType":"Internal",'.
           05  FILLER                  PIC X(50) VALUE
               '"publicNetworkAccess":"Disabled"}}'. 
       
       01  WS-REQUEST-JSON             PIC X(2048).
       
       PROCEDURE DIVISION USING LS-AZURE-CONFIG
                               LS-AUTH-TOKEN  
                               LS-RETURN-CODE.
       
       MAIN-PROCEDURE.
           DISPLAY "API Management deployment module starting..."
           MOVE ZERO TO LS-RETURN-CODE
           
           IF APIM-DEPLOY-FLAG = 'Y'
               PERFORM DEPLOY-API-MANAGEMENT
           ELSE
               DISPLAY "API Management deployment skipped (APIM_DEPLOY=N)"
           END-IF
           
           GOBACK.
       
       DEPLOY-API-MANAGEMENT.
           DISPLAY "Deploying Azure API Management..."
           
      *> Generate APIM name using naming convention
           PERFORM GENERATE-APIM-NAME
           
      *> Build ARM API URL
           PERFORM BUILD-APIM-URL
           
      *> Prepare JSON payload
           PERFORM PREPARE-JSON-PAYLOAD
           
      *> Make ARM API call
           PERFORM MAKE-API-CALL
           
           IF HTTP-STATUS-CODE = 200 OR HTTP-STATUS-CODE = 201
               DISPLAY "API Management created successfully: " WS-APIM-NAME
           ELSE
               DISPLAY "Failed to create API Management. Status: " 
                       HTTP-STATUS-CODE
               MOVE 1 TO LS-RETURN-CODE
           END-IF.
       
       GENERATE-APIM-NAME.
      *> Create APIM name using naming convention
           STRING 'apim-'
                  ENVIRONMENT
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO WS-APIM-NAME
           END-STRING
           
      *> Convert to lowercase
           INSPECT WS-APIM-NAME CONVERTING 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO 
               'abcdefghijklmnopqrstuvwxyz'.
       
       BUILD-APIM-URL.
           STRING 'https://management.azure.com/subscriptions/'
                  SUBSCRIPTION-ID
                  '/resourceGroups/'
                  RG-NAME
                  '/providers/Microsoft.ApiManagement/service/'
                  WS-APIM-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO WS-APIM-URL
           END-STRING.
       
       PREPARE-JSON-PAYLOAD.
      *> Fill in the JSON template with actual values
           MOVE RG-LOCATION TO JSON-LOCATION
           MOVE APIM-SKU-NAME TO JSON-SKU-NAME
           MOVE APIM-SKU-CAPACITY TO JSON-SKU-CAPACITY
           MOVE APIM-PUBLISHER-NAME TO JSON-PUBLISHER-NAME
           MOVE APIM-PUBLISHER-EMAIL TO JSON-PUBLISHER-EMAIL
           
      *> Convert template to actual JSON string
           STRING WS-JSON-TEMPLATE DELIMITED BY LOW-VALUE
               INTO WS-REQUEST-JSON
           END-STRING.
       
       MAKE-API-CALL.
      *> Set up HTTP request for ARM API
           MOVE 'PUT' TO HTTP-METHOD
           MOVE WS-APIM-URL TO HTTP-URL
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
           DISPLAY "  URL: " WS-APIM-URL
           DISPLAY "  Body: " WS-REQUEST-JSON(1:200) "...".
       
       END PROGRAM APIMGMT.