       IDENTIFICATION DIVISION.
       PROGRAM-ID. AZUREDEPLOY.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Main program for deploying Azure Integration Landing Zone resources
      *> This program orchestrates the deployment of all required Azure resources
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "deployment.log"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-RECORD                  PIC X(120).
       
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  WS-DEPLOYMENT-STATUS        PIC X(8) VALUE 'STARTING'.
       01  WS-CURRENT-RESOURCE         PIC X(32).
       01  WS-RETURN-CODE              PIC 9(2).
       01  WS-TIMESTAMP                PIC X(19).
       01  WS-LOG-MESSAGE              PIC X(120).
       
      *> Resource deployment flags
       01  DEPLOYMENT-FLAGS.
           05  LOG-ANALYTICS-DEPLOYED  PIC X VALUE 'N'.
           05  VNET-DEPLOYED           PIC X VALUE 'N'.
           05  KEY-VAULT-DEPLOYED      PIC X VALUE 'N'.
           05  STORAGE-DEPLOYED        PIC X VALUE 'N'.
           05  APIM-DEPLOYED           PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DEPLOYMENT
           PERFORM AUTHENTICATE-AZURE
           
           IF WS-RETURN-CODE = 0
               PERFORM DEPLOY-RESOURCES
               PERFORM FINALIZE-DEPLOYMENT
           ELSE
               PERFORM HANDLE-AUTH-ERROR
           END-IF
           
           STOP RUN.
       
       INITIALIZE-DEPLOYMENT.
           DISPLAY "Azure Integration Landing Zone Deployment Starting..."
           ACCEPT WS-TIMESTAMP FROM DATE YYYYMMDD
           MOVE 'STARTING' TO WS-DEPLOYMENT-STATUS
           
           OPEN OUTPUT LOG-FILE
           STRING 'Deployment started at ' WS-TIMESTAMP
               DELIMITED BY SIZE INTO WS-LOG-MESSAGE
           END-STRING
           WRITE LOG-RECORD FROM WS-LOG-MESSAGE
           
           CALL 'AZUREAUTH' USING BY REFERENCE AZURE-CONFIG
                                               AZURE-AUTH-TOKEN
                                               WS-RETURN-CODE.
       
       AUTHENTICATE-AZURE.
           IF WS-RETURN-CODE = 0
               DISPLAY "Azure authentication successful"
               STRING 'Authentication successful' 
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
           ELSE
               DISPLAY "Azure authentication failed"
               STRING 'Authentication failed with code: ' WS-RETURN-CODE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
           END-IF.
       
       DEPLOY-RESOURCES.
           DISPLAY "Beginning resource deployment..."
           MOVE 'DEPLOYING' TO WS-DEPLOYMENT-STATUS
           
      *> Deploy resources in dependency order
           PERFORM DEPLOY-LOG-ANALYTICS
           PERFORM DEPLOY-VIRTUAL-NETWORK
           PERFORM DEPLOY-KEY-VAULT
           PERFORM DEPLOY-STORAGE-ACCOUNTS
           
           IF APIM-DEPLOY-FLAG = 'Y'
               PERFORM DEPLOY-API-MANAGEMENT
           END-IF.
       
       DEPLOY-LOG-ANALYTICS.
           MOVE 'Log Analytics Workspace' TO WS-CURRENT-RESOURCE
           DISPLAY "Deploying " WS-CURRENT-RESOURCE "..."
           
           CALL 'LOGANALYTICS' USING BY REFERENCE AZURE-CONFIG
                                                  AZURE-AUTH-TOKEN
                                                  WS-RETURN-CODE
           
           IF WS-RETURN-CODE = 0
               MOVE 'Y' TO LOG-ANALYTICS-DEPLOYED
               STRING 'Successfully deployed ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment successful"
           ELSE
               STRING 'Failed to deploy ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment failed"
           END-IF.
       
       DEPLOY-VIRTUAL-NETWORK.
           MOVE 'Virtual Network' TO WS-CURRENT-RESOURCE
           DISPLAY "Deploying " WS-CURRENT-RESOURCE "..."
           
           CALL 'VIRTUALNET' USING BY REFERENCE AZURE-CONFIG
                                               AZURE-AUTH-TOKEN
                                               WS-RETURN-CODE
           
           IF WS-RETURN-CODE = 0
               MOVE 'Y' TO VNET-DEPLOYED
               STRING 'Successfully deployed ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment successful"
           ELSE
               STRING 'Failed to deploy ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment failed"
           END-IF.
       
       DEPLOY-KEY-VAULT.
           MOVE 'Key Vault' TO WS-CURRENT-RESOURCE
           DISPLAY "Deploying " WS-CURRENT-RESOURCE "..."
           
           CALL 'KEYVAULT' USING BY REFERENCE AZURE-CONFIG
                                             AZURE-AUTH-TOKEN
                                             WS-RETURN-CODE
           
           IF WS-RETURN-CODE = 0
               MOVE 'Y' TO KEY-VAULT-DEPLOYED
               STRING 'Successfully deployed ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment successful"
           ELSE
               STRING 'Failed to deploy ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment failed"
           END-IF.
       
       DEPLOY-STORAGE-ACCOUNTS.
           MOVE 'Storage Accounts' TO WS-CURRENT-RESOURCE
           DISPLAY "Deploying " WS-CURRENT-RESOURCE "..."
           
           CALL 'STORAGE' USING BY REFERENCE AZURE-CONFIG
                                            AZURE-AUTH-TOKEN
                                            WS-RETURN-CODE
           
           IF WS-RETURN-CODE = 0
               MOVE 'Y' TO STORAGE-DEPLOYED
               STRING 'Successfully deployed ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment successful"
           ELSE
               STRING 'Failed to deploy ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment failed"
           END-IF.
       
       DEPLOY-API-MANAGEMENT.
           MOVE 'API Management' TO WS-CURRENT-RESOURCE
           DISPLAY "Deploying " WS-CURRENT-RESOURCE "..."
           
           CALL 'APIMGMT' USING BY REFERENCE AZURE-CONFIG
                                            AZURE-AUTH-TOKEN
                                            WS-RETURN-CODE
           
           IF WS-RETURN-CODE = 0
               MOVE 'Y' TO APIM-DEPLOYED
               STRING 'Successfully deployed ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment successful"
           ELSE
               STRING 'Failed to deploy ' WS-CURRENT-RESOURCE
                   DELIMITED BY SIZE INTO WS-LOG-MESSAGE
               END-STRING
               WRITE LOG-RECORD FROM WS-LOG-MESSAGE
               DISPLAY WS-CURRENT-RESOURCE " deployment failed"
           END-IF.
       
       FINALIZE-DEPLOYMENT.
           MOVE 'COMPLETED' TO WS-DEPLOYMENT-STATUS
           DISPLAY "Deployment completed"
           
           DISPLAY "Deployment Summary:"
           DISPLAY "Log Analytics: " LOG-ANALYTICS-DEPLOYED
           DISPLAY "Virtual Network: " VNET-DEPLOYED
           DISPLAY "Key Vault: " KEY-VAULT-DEPLOYED
           DISPLAY "Storage Accounts: " STORAGE-DEPLOYED
           DISPLAY "API Management: " APIM-DEPLOYED
           
           STRING 'Deployment completed with status: ' WS-DEPLOYMENT-STATUS
               DELIMITED BY SIZE INTO WS-LOG-MESSAGE
           END-STRING
           WRITE LOG-RECORD FROM WS-LOG-MESSAGE
           
           CLOSE LOG-FILE.
       
       HANDLE-AUTH-ERROR.
           MOVE 'FAILED' TO WS-DEPLOYMENT-STATUS
           DISPLAY "Deployment failed due to authentication error"
           
           STRING 'Deployment failed - authentication error'
               DELIMITED BY SIZE INTO WS-LOG-MESSAGE
           END-STRING
           WRITE LOG-RECORD FROM WS-LOG-MESSAGE
           
           CLOSE LOG-FILE.
       
       END PROGRAM AZUREDEPLOY.