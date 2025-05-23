      *> HTTP Client Copybook for Azure REST API calls
      *> This copybook defines data structures for HTTP operations
       01  HTTP-REQUEST.
           05  HTTP-METHOD            PIC X(10).
           05  HTTP-URL               PIC X(512).
           05  HTTP-HEADERS.
               10  AUTHORIZATION      PIC X(1024).
               10  CONTENT-TYPE       PIC X(64).
               10  CONTENT-LENGTH     PIC 9(8).
           05  HTTP-BODY              PIC X(8192).
           
       01  HTTP-RESPONSE.
           05  HTTP-STATUS-CODE       PIC 9(3).
           05  HTTP-STATUS-TEXT       PIC X(64).
           05  HTTP-RESPONSE-HEADERS  PIC X(2048).
           05  HTTP-RESPONSE-BODY     PIC X(8192).
           
       01  AZURE-AUTH-TOKEN.
           05  ACCESS-TOKEN           PIC X(2048).
           05  TOKEN-TYPE             PIC X(16).
           05  EXPIRES-IN             PIC 9(8).
           05  TOKEN-EXPIRY-TIME      PIC 9(14).
           
       01  ARM-API-ENDPOINTS.
           05  RESOURCE-GROUPS-API    PIC X(256) VALUE
               'https://management.azure.com/subscriptions/'.
           05  STORAGE-ACCOUNTS-API   PIC X(256) VALUE
               'https://management.azure.com/subscriptions/'.
           05  KEY-VAULT-API          PIC X(256) VALUE
               'https://management.azure.com/subscriptions/'.
           05  VNET-API               PIC X(256) VALUE
               'https://management.azure.com/subscriptions/'.
           05  LOG-ANALYTICS-API      PIC X(256) VALUE
               'https://management.azure.com/subscriptions/'.
           05  AUTH-ENDPOINT          PIC X(256) VALUE
               'https://login.microsoftonline.com/'.
