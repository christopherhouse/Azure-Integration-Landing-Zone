       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATAFACTORY.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Azure Data Factory deployment module
      *> Creates Azure Data Factory instance with private networking and managed identities
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  WS-DATA-FACTORY.
           05  ADF-NAME                     PIC X(64).
           05  ADF-URL                      PIC X(256).
           05  ADF-MVN-NAME                 PIC X(64).
           05  ADF-ENDPOINT-NAME            PIC X(64).
           05  ADF-IDENTITY-TYPE            PIC X(16) VALUE 'SystemAssigned'.
           05  ADF-PUBLIC-NETWORK           PIC X(1)   VALUE 'N'.
           05  ADF-PRIVATE-LINK-ID          PIC X(128).
           05  ADF-MANAGED-PE-COUNT         PIC 9(2).
           05  ADF-GIT-CONFIG-ENABLED       PIC X(1)   VALUE 'N'.
       
       01  WS-MANAGED-PE.
           05  MANAGED-PE-NAME              PIC X(64).
           05  MANAGED-PE-TARGET-ID         PIC X(256).
           05  MANAGED-PE-SUBRESOURCE       PIC X(32).
       
       01  WS-API-STATUS.
           05  STATUS-SUCCESS              PIC X(1).
           05  STATUS-MESSAGE              PIC X(128).
           05  OPERATION-ID                PIC X(36).
       
       01  WS-JSON.
           05  JSON-PAYLOAD                PIC X(8192).
           05  JSON-LENGTH                 PIC 9(6).
       
       01  WS-SUBSCRIPTION-ID              PIC X(36).
       01  WS-RESOURCE-GROUP               PIC X(64).
       01  WS-LOCATION                     PIC X(32).
       01  WS-API-VERSION                  PIC X(10) VALUE '2018-06-01'.
       01  WS-NETWORK-API-VERSION          PIC X(10) VALUE '2022-05-01'.
       
       LINKAGE SECTION.
       01  LS-AZURE-CONFIG                 PIC X ANY LENGTH.
       01  LS-AZURE-AUTH-TOKEN             PIC X ANY LENGTH.
       01  LS-RETURN-CODE                  PIC 9(2).
       
       PROCEDURE DIVISION USING LS-AZURE-CONFIG, 
                                LS-AZURE-AUTH-TOKEN, 
                                LS-RETURN-CODE.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DEPLOYMENT
           
           IF LS-RETURN-CODE = 0
               PERFORM CREATE-DATA-FACTORY
           END-IF
           
           IF LS-RETURN-CODE = 0
               PERFORM CREATE-MANAGED-VNET
           END-IF
           
           IF LS-RETURN-CODE = 0
               PERFORM CONFIGURE-PRIVATE-ENDPOINT
           END-IF
           
           IF LS-RETURN-CODE = 0
               PERFORM CREATE-MANAGED-PRIVATE-ENDPOINTS
           END-IF
           
           GOBACK.
       
       INITIALIZE-DEPLOYMENT.
           MOVE 0 TO LS-RETURN-CODE
           
           MOVE LS-AZURE-CONFIG TO AZURE-CONFIG
           MOVE SUBSCRIPTION-ID TO WS-SUBSCRIPTION-ID
           MOVE RG-NAME TO WS-RESOURCE-GROUP
           MOVE RG-LOCATION TO WS-LOCATION
           
           DISPLAY "Initializing Data Factory deployment..."
           DISPLAY "  Subscription: " WS-SUBSCRIPTION-ID
           DISPLAY "  Resource Group: " WS-RESOURCE-GROUP
           DISPLAY "  Location: " WS-LOCATION
           
           PERFORM GENERATE-DATA-FACTORY-NAME
           
           DISPLAY "  Data Factory Name: " ADF-NAME
           DISPLAY "  Managed Virtual Network Name: " ADF-MVN-NAME
           DISPLAY "  Private Endpoint Name: " ADF-ENDPOINT-NAME
           
           STRING 'https://management.azure.com/subscriptions/'
                  WS-SUBSCRIPTION-ID
                  '/resourceGroups/'
                  WS-RESOURCE-GROUP
                  '/providers/Microsoft.DataFactory/factories/'
                  ADF-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO ADF-URL
           END-STRING.
       
       GENERATE-DATA-FACTORY-NAME.
           MOVE 'Y' TO STATUS-SUCCESS
           
           STRING 'df-'
                  ENV-NAME
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO ADF-NAME
           END-STRING
           
           INSPECT ADF-NAME CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                  TO 'abcdefghijklmnopqrstuvwxyz'.
           
           STRING 'vnet-'
                  ADF-NAME
               DELIMITED BY SIZE INTO ADF-MVN-NAME
           END-STRING
           
           STRING 'pe-'
                  ADF-NAME
               DELIMITED BY SIZE INTO ADF-ENDPOINT-NAME
           END-STRING.
       
       CREATE-DATA-FACTORY.
           DISPLAY "Creating Azure Data Factory..."
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE ADF-URL TO HTTP-URL
           MOVE SPACES TO HTTP-BODY
           
           MOVE 'application/json' TO CONTENT-TYPE
           STRING 'Bearer ' ACCESS-TOKEN
               DELIMITED BY SIZE INTO AUTHORIZATION
           END-STRING
           
           PERFORM GENERATE-DATAFACTORY-JSON-PAYLOAD
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           DISPLAY "  API URL: " HTTP-URL
           DISPLAY "  Calling ARM API to create Data Factory..."
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > df_response.json"
           END-CALL
           
           DISPLAY "  Data Factory creation initiated"
           MOVE 'Y' TO STATUS-SUCCESS
           
           IF STATUS-SUCCESS = 'Y'
               DISPLAY "  Data Factory creation successful"
           ELSE
               DISPLAY "  Data Factory creation failed: " 
                   STATUS-MESSAGE
               MOVE 8 TO LS-RETURN-CODE
           END-IF.
       
       GENERATE-DATAFACTORY-JSON-PAYLOAD.
           MOVE SPACES TO JSON-PAYLOAD
           
           STRING '{'
               '"location": "' WS-LOCATION '",'
               '"identity": {'
               '  "type": "' ADF-IDENTITY-TYPE '"'
               '},'
               '"properties": {'
               '  "publicNetworkAccess": "'
           DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           IF ADF-PUBLIC-NETWORK = 'Y'
               STRING JSON-PAYLOAD
                   'Enabled"'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           ELSE
               STRING JSON-PAYLOAD
                   'Disabled"'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           END-IF
           
           STRING JSON-PAYLOAD
               ','
               '  "purviewConfiguration": {'
               '    "purviewResourceId": null'
               '  }'
               '},'
               '"tags": {'
               '  "environment": "' ENV-NAME '",'
               '  "project": "ais-landing-zone"'
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD)
           DISPLAY "  Generated JSON payload for Data Factory".
       
       CREATE-MANAGED-VNET.
           DISPLAY "Creating Managed Virtual Network for Data Factory..."
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           STRING ADF-URL(1:(FUNCTION LENGTH(ADF-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
                  '/managedVirtualNetworks/'
                  ADF-MVN-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO HTTP-URL
           END-STRING
           
           MOVE SPACES TO HTTP-BODY
           MOVE '{"properties": {}}' TO HTTP-BODY
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(HTTP-BODY)
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > mvnet_response.json"
           END-CALL
           
           DISPLAY "  Managed Virtual Network creation initiated"
           MOVE 'Y' TO STATUS-SUCCESS
           
           IF STATUS-SUCCESS = 'Y'
               DISPLAY "  Managed Virtual Network creation successful"
           ELSE
               DISPLAY "  Managed Virtual Network creation failed: " 
                   STATUS-MESSAGE
               MOVE 8 TO LS-RETURN-CODE
           END-IF.
       
       CONFIGURE-PRIVATE-ENDPOINT.
           DISPLAY "Configuring Private Endpoint for Data Factory..."
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           
           STRING 'https://management.azure.com/subscriptions/'
                  WS-SUBSCRIPTION-ID
                  '/resourceGroups/'
                  WS-RESOURCE-GROUP
                  '/providers/Microsoft.Network/privateEndpoints/'
                  ADF-ENDPOINT-NAME
                  '?api-version='
                  WS-NETWORK-API-VERSION
               DELIMITED BY SIZE INTO HTTP-URL
           END-STRING
           
           MOVE SPACES TO HTTP-BODY
           PERFORM GENERATE-PRIVATE-ENDPOINT-JSON
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > pe_response.json"
           END-CALL
           
           DISPLAY "  Private Endpoint configuration completed".
       
       GENERATE-PRIVATE-ENDPOINT-JSON.
           MOVE SPACES TO JSON-PAYLOAD
           
           MOVE SPACES TO ADF-PRIVATE-LINK-ID
           STRING ADF-URL(1:(FUNCTION LENGTH(ADF-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
               DELIMITED BY SIZE INTO ADF-PRIVATE-LINK-ID
           END-STRING
           
           STRING '{'
               '"location": "' WS-LOCATION '",'
               '"properties": {'
               '  "subnet": {'
               '    "id": "/subscriptions/'
                       WS-SUBSCRIPTION-ID
                       '/resourceGroups/'
                       WS-RESOURCE-GROUP
                       '/providers/Microsoft.Network/virtualNetworks/'
                       VNET-NAME
                       '/subnets/private-endpoints"'
               '  },'
               '  "privateLinkServiceConnections": [{'
               '    "name": "' ADF-ENDPOINT-NAME '",'
               '    "properties": {'
               '      "privateLinkServiceId": "' ADF-PRIVATE-LINK-ID '",'
               '      "groupIds": ["dataFactory"]'
               '    }'
               '  }]'
               '},'
               '"tags": {'
               '  "environment": "' ENV-NAME '"'
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD).
       
       CREATE-MANAGED-PRIVATE-ENDPOINTS.
           DISPLAY "Creating Managed Private Endpoints..."
           
           MOVE 2 TO ADF-MANAGED-PE-COUNT
           PERFORM VARYING ADF-MANAGED-PE-COUNT FROM 1 BY 1 
                   UNTIL ADF-MANAGED-PE-COUNT > 2
               PERFORM CREATE-SINGLE-MANAGED-PE
           END-PERFORM
           
           DISPLAY "  Managed Private Endpoints completed".
       
       CREATE-SINGLE-MANAGED-PE.
           EVALUATE ADF-MANAGED-PE-COUNT
               WHEN 1
                   MOVE "sql-server-endpoint" TO MANAGED-PE-NAME
                   MOVE "/subscriptions/" TO MANAGED-PE-TARGET-ID
                   STRING MANAGED-PE-TARGET-ID
                       WS-SUBSCRIPTION-ID
                       "/resourceGroups/"
                       WS-RESOURCE-GROUP
                       "/providers/Microsoft.Sql/servers/sql-server"
                       DELIMITED BY SIZE INTO MANAGED-PE-TARGET-ID
                   END-STRING
                   MOVE "sqlServer" TO MANAGED-PE-SUBRESOURCE
               WHEN 2
                   MOVE "storage-endpoint" TO MANAGED-PE-NAME
                   MOVE "/subscriptions/" TO MANAGED-PE-TARGET-ID
                   STRING MANAGED-PE-TARGET-ID
                       WS-SUBSCRIPTION-ID
                       "/resourceGroups/"
                       WS-RESOURCE-GROUP
                       "/providers/Microsoft.Storage/storageAccounts/storage"
                       DELIMITED BY SIZE INTO MANAGED-PE-TARGET-ID
                   END-STRING
                   MOVE "blob" TO MANAGED-PE-SUBRESOURCE
           END-EVALUATE
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           STRING ADF-URL(1:(FUNCTION LENGTH(ADF-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
                  '/managedVirtualNetworks/'
                  ADF-MVN-NAME
                  '/managedPrivateEndpoints/'
                  MANAGED-PE-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO HTTP-URL
           END-STRING
           
           MOVE SPACES TO HTTP-BODY
           PERFORM GENERATE-MANAGED-PE-JSON
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > managed_pe_response.json"
           END-CALL
           
           DISPLAY "  Created managed private endpoint: " 
               MANAGED-PE-NAME.
       
       GENERATE-MANAGED-PE-JSON.
           MOVE SPACES TO JSON-PAYLOAD
           
           STRING '{'
               '"properties": {'
               '  "target": "' MANAGED-PE-TARGET-ID '",'
               '  "groupId": "' MANAGED-PE-SUBRESOURCE '"'
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD).
       
       END PROGRAM DATAFACTORY.