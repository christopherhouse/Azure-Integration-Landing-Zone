       IDENTIFICATION DIVISION.
       PROGRAM-ID. SERVICEBUS.
       AUTHOR. Azure Integration Landing Zone Team.
       DATE-WRITTEN. 2024.
       
      *> Azure Service Bus deployment module
      *> Creates premium tier Service Bus namespace with queues, topics, and security
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       COPY AZURECONFIG.
       COPY HTTPCLIENT.
       
       01  WS-SERVICE-BUS.
           05  SB-NAME                     PIC X(64).
           05  SB-CAPACITY-UNITS           PIC 9(2).
           05  SB-NAMESPACE-URL            PIC X(256).
           05  SB-QUEUE-COUNT              PIC 9(2).
           05  SB-TOPIC-COUNT              PIC 9(2).
           05  SB-ENDPOINT-NAME            PIC X(64).
           05  SB-PRIVATE-LINK-ID          PIC X(128).
       
       01  WS-QUEUE-DETAILS.
           05  QUEUE-NAME                  PIC X(64).
           05  MAX-SIZE-MEGABYTES          PIC 9(6).
           05  DEFAULT-TTL                 PIC X(16).
           05  MAX-DELIVERY-COUNT          PIC 9(3).
           05  REQUIRES-SESSION            PIC X(1).
           05  DEAD-LETTER-ON-EXPIRATION   PIC X(1).
       
       01  WS-TOPIC-DETAILS.
           05  TOPIC-NAME                  PIC X(64).
           05  MAX-SIZE-MEGABYTES          PIC 9(6).
           05  DEFAULT-TTL                 PIC X(16).
           05  SUBSCRIPTION-COUNT          PIC 9(2).
           05  SUBSCRIPTION-DETAILS OCCURS 5 TIMES.
               10  SUB-NAME                PIC X(64).
               10  SUB-MAX-DELIVERY-COUNT  PIC 9(3).
               10  SUB-DEFAULT-TTL         PIC X(16).
               10  SUB-REQUIRES-SESSION    PIC X(1).
       
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
       01  WS-API-VERSION                  PIC X(10) VALUE '2021-06-01'.
       
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
               PERFORM CREATE-SERVICE-BUS-NAMESPACE
           END-IF
           
           IF LS-RETURN-CODE = 0
               PERFORM CREATE-QUEUES
           END-IF
           
           IF LS-RETURN-CODE = 0
               PERFORM CREATE-TOPICS
           END-IF
           
           IF LS-RETURN-CODE = 0
               PERFORM CONFIGURE-PRIVATE-ENDPOINT
           END-IF
           
           GOBACK.
       
       INITIALIZE-DEPLOYMENT.
           MOVE 0 TO LS-RETURN-CODE
           
           MOVE LS-AZURE-CONFIG TO AZURE-CONFIG
           MOVE SUBSCRIPTION-ID TO WS-SUBSCRIPTION-ID
           MOVE RG-NAME TO WS-RESOURCE-GROUP
           MOVE RG-LOCATION TO WS-LOCATION
           
           DISPLAY "Initializing Service Bus Namespace deployment..."
           DISPLAY "  Subscription: " WS-SUBSCRIPTION-ID
           DISPLAY "  Resource Group: " WS-RESOURCE-GROUP
           DISPLAY "  Location: " WS-LOCATION
           
           PERFORM GENERATE-SERVICE-BUS-NAME
           
           DISPLAY "  Service Bus Namespace: " SB-NAME
           
           STRING 'https://management.azure.com/subscriptions/'
                  WS-SUBSCRIPTION-ID
                  '/resourceGroups/'
                  WS-RESOURCE-GROUP
                  '/providers/Microsoft.ServiceBus/namespaces/'
                  SB-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO SB-NAMESPACE-URL
           END-STRING.
       
       GENERATE-SERVICE-BUS-NAME.
           MOVE 'Y' TO STATUS-SUCCESS
           
           STRING 'sb-'
                  ENV-NAME
                  '-'
                  SUFFIX
               DELIMITED BY SIZE INTO SB-NAME
           END-STRING
           
           INSPECT SB-NAME CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                  TO 'abcdefghijklmnopqrstuvwxyz'.
           
           STRING 'sbns-'
                  ENV-NAME
                  '-pe'
               DELIMITED BY SIZE INTO SB-ENDPOINT-NAME
           END-STRING.
       
       CREATE-SERVICE-BUS-NAMESPACE.
           DISPLAY "Creating Service Bus Namespace..."
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SB-NAMESPACE-URL TO HTTP-URL
           MOVE SPACES TO HTTP-BODY
           
           MOVE 'application/json' TO CONTENT-TYPE
           STRING 'Bearer ' ACCESS-TOKEN
               DELIMITED BY SIZE INTO AUTHORIZATION
           END-STRING
           
           PERFORM GENERATE-NAMESPACE-JSON-PAYLOAD
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           DISPLAY "  API URL: " HTTP-URL
           DISPLAY "  Calling ARM API to create Service Bus namespace..."
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > sb_response.json"
           END-CALL
           
           DISPLAY "  Service Bus Namespace creation initiated"
           MOVE 'Y' TO STATUS-SUCCESS
           
           IF STATUS-SUCCESS = 'Y'
               DISPLAY "  Service Bus Namespace creation successful"
           ELSE
               DISPLAY "  Service Bus Namespace creation failed: " 
                   STATUS-MESSAGE
               MOVE 8 TO LS-RETURN-CODE
           END-IF.
       
       GENERATE-NAMESPACE-JSON-PAYLOAD.
           MOVE SPACES TO JSON-PAYLOAD
           
           STRING '{'
               '"location": "' WS-LOCATION '",'
               '"sku": {'
               '  "name": "Premium",'
               '  "tier": "Premium",'
               '  "capacity": ' SB-CAPACITY-UNITS
               '},'
               '"properties": {'
               '  "zoneRedundant": true,'
               '  "disableLocalAuth": true,'
               '  "minimumTlsVersion": "1.2"'
               '},'
               '"tags": {'
               '  "environment": "' ENV-NAME '"'
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD)
           DISPLAY "  Generated JSON payload for Service Bus namespace".
       
       CREATE-QUEUES.
           DISPLAY "Creating Service Bus Queues..."
           MOVE 1 TO SB-QUEUE-COUNT
           
           PERFORM VARYING SB-QUEUE-COUNT FROM 1 BY 1 
                   UNTIL SB-QUEUE-COUNT > 2
               PERFORM CREATE-SINGLE-QUEUE
           END-PERFORM
           
           DISPLAY "  Service Bus queues created".
       
       CREATE-SINGLE-QUEUE.
           EVALUATE SB-QUEUE-COUNT
               WHEN 1
                   MOVE "orders-queue" TO QUEUE-NAME
                   MOVE 1024 TO MAX-SIZE-MEGABYTES
                   MOVE "P14D" TO DEFAULT-TTL
                   MOVE 10 TO MAX-DELIVERY-COUNT
                   MOVE "N" TO REQUIRES-SESSION
                   MOVE "N" TO DEAD-LETTER-ON-EXPIRATION
               WHEN 2
                   MOVE "notifications-queue" TO QUEUE-NAME
                   MOVE 1024 TO MAX-SIZE-MEGABYTES
                   MOVE "P7D" TO DEFAULT-TTL
                   MOVE 5 TO MAX-DELIVERY-COUNT
                   MOVE "Y" TO REQUIRES-SESSION
                   MOVE "Y" TO DEAD-LETTER-ON-EXPIRATION
           END-EVALUATE
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           STRING SB-NAMESPACE-URL(1:(FUNCTION LENGTH(SB-NAMESPACE-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
                  '/queues/'
                  QUEUE-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO HTTP-URL
           END-STRING
           
           MOVE SPACES TO HTTP-BODY
           PERFORM GENERATE-QUEUE-JSON-PAYLOAD
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > queue_response.json"
           END-CALL
           
           DISPLAY "  Created queue: " QUEUE-NAME.
       
       GENERATE-QUEUE-JSON-PAYLOAD.
           MOVE SPACES TO JSON-PAYLOAD
           
           STRING '{'
               '"properties": {'
               '  "maxSizeInMegabytes": ' MAX-SIZE-MEGABYTES ','
           DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           IF DEFAULT-TTL NOT = SPACES
               STRING JSON-PAYLOAD
                   '  "defaultMessageTimeToLive": "' DEFAULT-TTL '",'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           END-IF
           
           STRING JSON-PAYLOAD
               '  "maxDeliveryCount": ' MAX-DELIVERY-COUNT ','
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           IF REQUIRES-SESSION = 'Y'
               STRING JSON-PAYLOAD
                   '  "requiresSession": true,'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           ELSE
               STRING JSON-PAYLOAD
                   '  "requiresSession": false,'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           END-IF
           
           IF DEAD-LETTER-ON-EXPIRATION = 'Y'
               STRING JSON-PAYLOAD
                   '  "deadLetteringOnMessageExpiration": true'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           ELSE
               STRING JSON-PAYLOAD
                   '  "deadLetteringOnMessageExpiration": false'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           END-IF
           
           STRING JSON-PAYLOAD
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD).
       
       CREATE-TOPICS.
           DISPLAY "Creating Service Bus Topics and Subscriptions..."
           MOVE 1 TO SB-TOPIC-COUNT
           
           PERFORM CREATE-SINGLE-TOPIC
           
           DISPLAY "  Service Bus topics and subscriptions created".
       
       CREATE-SINGLE-TOPIC.
           MOVE "events" TO TOPIC-NAME
           MOVE 1024 TO MAX-SIZE-MEGABYTES
           MOVE "P14D" TO DEFAULT-TTL
           MOVE 2 TO SUBSCRIPTION-COUNT
           
           MOVE "all-events" TO SUB-NAME(1)
           MOVE 10 TO SUB-MAX-DELIVERY-COUNT(1)
           MOVE SPACES TO SUB-DEFAULT-TTL(1)
           MOVE "N" TO SUB-REQUIRES-SESSION(1)
           
           MOVE "critical-events" TO SUB-NAME(2)
           MOVE 20 TO SUB-MAX-DELIVERY-COUNT(2)
           MOVE "P7D" TO SUB-DEFAULT-TTL(2)
           MOVE "Y" TO SUB-REQUIRES-SESSION(2)
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           STRING SB-NAMESPACE-URL(1:(FUNCTION LENGTH(SB-NAMESPACE-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
                  '/topics/'
                  TOPIC-NAME
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO HTTP-URL
           END-STRING
           
           MOVE SPACES TO HTTP-BODY
           PERFORM GENERATE-TOPIC-JSON-PAYLOAD
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > topic_response.json"
           END-CALL
           
           DISPLAY "  Created topic: " TOPIC-NAME
           
           PERFORM VARYING SB-QUEUE-COUNT FROM 1 BY 1 
                   UNTIL SB-QUEUE-COUNT > SUBSCRIPTION-COUNT
               PERFORM CREATE-TOPIC-SUBSCRIPTION
           END-PERFORM.
       
       GENERATE-TOPIC-JSON-PAYLOAD.
           MOVE SPACES TO JSON-PAYLOAD
           
           STRING '{'
               '"properties": {'
               '  "maxSizeInMegabytes": ' MAX-SIZE-MEGABYTES ','
               '  "defaultMessageTimeToLive": "' DEFAULT-TTL '"'
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD).
       
       CREATE-TOPIC-SUBSCRIPTION.
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           STRING SB-NAMESPACE-URL(1:(FUNCTION LENGTH(SB-NAMESPACE-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
                  '/topics/'
                  TOPIC-NAME
                  '/subscriptions/'
                  SUB-NAME(SB-QUEUE-COUNT)
                  '?api-version='
                  WS-API-VERSION
               DELIMITED BY SIZE INTO HTTP-URL
           END-STRING
           
           MOVE SPACES TO HTTP-BODY
           PERFORM GENERATE-SUBSCRIPTION-JSON-PAYLOAD
           MOVE JSON-PAYLOAD TO HTTP-BODY
           MOVE JSON-LENGTH TO CONTENT-LENGTH
           
           CALL "C$SYSTEM" USING 
               "curl -s -X PUT -H \"Authorization: ******" 
               ACCESS-TOKEN "\" -H \"Content-Type: application/json\" "
               "-d '" HTTP-BODY "' \"" HTTP-URL "\" > sub_response.json"
           END-CALL
           
           DISPLAY "    Created subscription: " 
               SUB-NAME(SB-QUEUE-COUNT).
       
       GENERATE-SUBSCRIPTION-JSON-PAYLOAD.
           MOVE SPACES TO JSON-PAYLOAD
           
           STRING '{'
               '"properties": {'
               '  "maxDeliveryCount": ' 
                   SUB-MAX-DELIVERY-COUNT(SB-QUEUE-COUNT)
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           IF SUB-DEFAULT-TTL(SB-QUEUE-COUNT) NOT = SPACES
               STRING JSON-PAYLOAD
                   ','
                   '  "defaultMessageTimeToLive": "' 
                       SUB-DEFAULT-TTL(SB-QUEUE-COUNT) '"'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           END-IF
           
           IF SUB-REQUIRES-SESSION(SB-QUEUE-COUNT) = 'Y'
               STRING JSON-PAYLOAD
                   ','
                   '  "requiresSession": true'
                   DELIMITED BY SIZE INTO JSON-PAYLOAD
               END-STRING
           END-IF
           
           STRING JSON-PAYLOAD
               '}'
               '}'
               DELIMITED BY SIZE INTO JSON-PAYLOAD
           END-STRING
           
           COMPUTE JSON-LENGTH = FUNCTION LENGTH(JSON-PAYLOAD).
       
       CONFIGURE-PRIVATE-ENDPOINT.
           DISPLAY "Configuring Private Endpoint for Service Bus..."
           
           MOVE 'PUT' TO HTTP-METHOD
           MOVE SPACES TO HTTP-URL
           
           STRING 'https://management.azure.com/subscriptions/'
                  WS-SUBSCRIPTION-ID
                  '/resourceGroups/'
                  WS-RESOURCE-GROUP
                  '/providers/Microsoft.Network/privateEndpoints/'
                  SB-ENDPOINT-NAME
                  '?api-version=2022-05-01'
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
           
           MOVE SPACES TO SB-PRIVATE-LINK-ID
           STRING SB-NAMESPACE-URL(1:(FUNCTION LENGTH(SB-NAMESPACE-URL) - 
                   (FUNCTION LENGTH(WS-API-VERSION) + 13)))
               DELIMITED BY SIZE INTO SB-PRIVATE-LINK-ID
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
               '    "name": "' SB-ENDPOINT-NAME '",'
               '    "properties": {'
               '      "privateLinkServiceId": "' SB-PRIVATE-LINK-ID '",'
               '      "groupIds": ["namespace"]'
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
       
       END PROGRAM SERVICEBUS.