      *> Azure Configuration Copybook
      *> This copybook defines the data structures for Azure resource configuration
       01  AZURE-CONFIG.
           05  SUBSCRIPTION-INFO.
               10  SUBSCRIPTION-ID         PIC X(36).
               10  TENANT-ID              PIC X(36).
               10  CLIENT-ID              PIC X(36).
               10  CLIENT-SECRET          PIC X(256).
           05  RESOURCE-GROUP-INFO.
               10  RG-NAME                PIC X(64).
               10  RG-LOCATION            PIC X(32).
           05  ENVIRONMENT-INFO.
               10  ENV-NAME               PIC X(16).
               10  SUFFIX                 PIC X(16).
           05  NETWORKING-INFO.
               10  VNET-NAME              PIC X(64).
               10  VNET-ADDRESS-SPACE     PIC X(32).
               10  SUBNET-COUNT           PIC 9(2).
               10  SUBNET-INFO OCCURS 10 TIMES.
                   15  SUBNET-NAME        PIC X(32).
                   15  SUBNET-PREFIX      PIC X(32).
           05  STORAGE-INFO.
               10  STORAGE-COUNT          PIC 9(2).
               10  STORAGE-ACCOUNTS OCCURS 5 TIMES.
                   15  SA-NAME-PREFIX     PIC X(32).
                   15  SA-SKU-NAME        PIC X(16).
                   15  SA-ACCOUNT-KIND    PIC X(16).
                   15  SA-ACCESS-TIER     PIC X(8).
           05  KEY-VAULT-INFO.
               10  KV-NAME                PIC X(64).
               10  KV-PURGE-PROTECTION    PIC X(1).
               10  KV-SOFT-DELETE-DAYS    PIC 9(3).
           05  LOG-ANALYTICS-INFO.
               10  LA-WORKSPACE-NAME      PIC X(64).
           05  API-MGMT-INFO.
               10  APIM-DEPLOY-FLAG       PIC X(1).
               10  APIM-NAME              PIC X(64).
               10  APIM-SKU-NAME          PIC X(16).
               10  APIM-SKU-CAPACITY      PIC 9(2).
               10  APIM-PUBLISHER-NAME    PIC X(64).
               10  APIM-PUBLISHER-EMAIL   PIC X(128).
           05  SERVICE-BUS-INFO.
               10  SB-DEPLOY-FLAG         PIC X(1).
               10  SB-NAME                PIC X(64).
               10  SB-CAPACITY-UNITS      PIC 9(2).
               10  SB-QUEUE-COUNT         PIC 9(2).
               10  SB-QUEUE-INFO OCCURS 5 TIMES.
                   15  SB-QUEUE-NAME      PIC X(32).
                   15  SB-QUEUE-SIZE-MB   PIC 9(6).
                   15  SB-QUEUE-TTL       PIC X(16).
                   15  SB-QUEUE-DELIVERY  PIC 9(3).
               10  SB-TOPIC-COUNT         PIC 9(2).
               10  SB-TOPIC-INFO OCCURS 5 TIMES.
                   15  SB-TOPIC-NAME      PIC X(32).
                   15  SB-TOPIC-SIZE-MB   PIC 9(6).
                   15  SB-TOPIC-TTL       PIC X(16).
           05  DATA-FACTORY-INFO.
               10  DF-DEPLOY-FLAG         PIC X(1).
               10  DF-NAME                PIC X(64).
               10  DF-PUBLIC-NETWORK      PIC X(1).
               10  DF-MANAGED-VNET        PIC X(1).
               10  DF-IDENTITY-TYPE       PIC X(16).
               10  DF-GIT-ENABLED         PIC X(1).
               10  DF-MANAGED-PE-COUNT    PIC 9(2).
               10  DF-MANAGED-PE-INFO OCCURS 5 TIMES.
                   15  DF-PE-NAME         PIC X(32).
                   15  DF-PE-TARGET-ID    PIC X(128).
                   15  DF-PE-SUBRESOURCE  PIC X(16).
