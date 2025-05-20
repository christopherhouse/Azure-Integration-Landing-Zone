resource_group_name = "RG-AIS-LZ-TF"
location            = "eastus2"
subscription_id = "c5d4a6e8-69bf-4148-be25-cb362f83c370"
suffix       = "lz-tf"
environment  = "dev"

vnet_address_spaces = [
  "10.10.0.0/16"
]

vnet_subnets = [
  {
    name             = "ase"
    address_prefixes = ["10.10.1.0/24"]
    route_table      = null
    delegation = {
      name         = "ase-delegation"
      service_name = "Microsoft.Web/hostingEnvironments"
      actions      = ["Microsoft.Network/virtualNetworks/subnets/action"]
    }
    service_endpoints = []
  },
  {
    name             = "private-endpoints"
    address_prefixes = ["10.10.2.0/24"]
    nsg              = null
    route_table      = null
    delegation       = null
    service_endpoints = []
  },
  {
    name             = "apim"
    address_prefixes = ["10.10.3.0/24"]
    nsg = {
      name           = "apim-nsg"
      security_rules = [
        // Inbound rules
        {
          name                         = "AllowApimManagementInbound"
          priority                     = 100
          direction                    = "Inbound"
          access                       = "Allow"
          protocol                     = "Tcp"
          source_port_range            = "*"
          destination_port_range       = "3443"
          source_address_prefix        = "ApiManagement"
          destination_address_prefix   = "VirtualNetwork"
          description                  = "Allow inbound management traffic from Azure API Management service on port 3443"
        },
        {
          name                         = "AllowAzureLoadBalancerInBound"
          priority                     = 200
          direction                    = "Inbound"
          access                       = "Allow"
          protocol                     = "Tcp"
          source_port_range            = "*"
          destination_port_range       = "6390"
          source_address_prefix        = "AzureLoadBalancer"
          destination_address_prefix   = "VirtualNetwork"
          description                  = "Allow inbound health probe traffic from Azure Load Balancer on port 6390"
        },
        {
          name                         = "AzureTrafficManagerInbound"
          priority                     = 300
          direction                    = "Inbound"
          access                       = "Allow"
          protocol                     = "Tcp"
          source_port_range            = "*"
          destination_port_range       = "443"
          source_address_prefix        = "AzureTrafficManager"
          destination_address_prefix   = "Virtualnetwork"
          description                  = "Allow inbound traffic from Azure Traffic Manager on port 443"
        },
        // Outbound rules
        {
          name                         = "AllowStorageOutbound"
          priority                     = 400
          direction                    = "Outbound"
          access                       = "Allow"
          protocol                     = "Tcp"
          source_port_range            = "*"
          destination_port_range       = "443"
          source_address_prefix        = "VirtualNetwork"
          destination_address_prefix   = "Storage"
          description                  = "Allow outbound traffic to Azure Storage on port 443"
        },
        {
          name                         = "AllowSqlOutbound"
          priority                     = 500
          direction                    = "Outbound"
          access                       = "Allow"
          protocol                     = "Tcp"
          source_port_range            = "*"
          destination_port_range       = "1433"
          source_address_prefix        = "VirtualNetwork"
          destination_address_prefix   = "SQL"
          description                  = "Allow outbound traffic to Azure SQL Database on port 1433"
        },
        {
          name                         = "AllowAzureKeyVaultOutbound"
          priority                     = 600
          direction                    = "Outbound"
          access                       = "Allow"
          protocol                     = "Tcp"
          source_port_range            = "*"
          destination_port_range       = "443"
          source_address_prefix        = "VirtualNetwork"
          destination_address_prefix   = "AzureKeyVault"
          description                  = "Allow outbound traffic to Azure Key Vault on port 443"
        },
        {
          name                       = "AllowAzureMonitorOutbound1886"
          priority                   = 700
          direction                  = "Outbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "1886"
          source_address_prefix      = "VirtualNetwork"
          destination_address_prefix = "AzureMonitor"
          description                = "Allow outbound traffic to Azure Monitor on port 1886"
        },
        {
          name                       = "AllowAzureMonitorOutbound443"
          priority                   = 800
          direction                  = "Outbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "443"
          source_address_prefix      = "VirtualNetwork"
          destination_address_prefix = "AzureMonitor"
          description                = "Allow outbound traffic to Azure Monitor on port 443"
        }
      ]
      diag_enabled = true
    }
    route_table      = null
    delegation       = null
    service_endpoints = []
  }
]

key_vault_purge_protection_enabled = true
key_vault_soft_delete_retention_days = 7

apim_publisher_name  = "Contoso"
apim_publisher_email = "apis@contoso.net"
apim_sku_name     = "Developer"
apim_sku_capacity = 1

deploy_api_management = true
deploy_app_service_environment = true
deploy_service_bus = true
service_bus_capacity_units = 1
service_bus_queues = [
  {
    name = "orders-queue"
    max_size_in_megabytes = 1024
    default_message_ttl = "P14D"  # 14 days
    max_delivery_count = 10
  },
  {
    name = "notifications-queue"
    max_size_in_megabytes = 1024
    default_message_ttl = "P7D"   # 7 days
    max_delivery_count = 5
  }
]
service_bus_topics = [
  {
    name = "events"
    max_size_in_megabytes = 1024
    default_message_ttl = "P14D"  # 14 days
    subscriptions = [
      {
        name = "all-events"
        max_delivery_count = 10
      },
      {
        name = "critical-events"
        max_delivery_count = 20
        default_message_ttl = "P7D"  # 7 days
      }
    ]
  },
  {
    name = "alerts"
    max_size_in_megabytes = 1024
    subscriptions = [
      {
        name = "system-alerts"
        max_delivery_count = 10
      },
      {
        name = "security-alerts"
        max_delivery_count = 10
        dead_lettering_on_message_expiration = true
      }
    ]
  }
]

storage_accounts = [
  {
    name_prefix = "apimbackup"
    private_endpoints = ["blob"]
    create_private_dns_zone = true
    blob_containers = [
      {
        name = "apimbackup"
        public_access = "None"
      }
    ]
    tables = []
    queues = []
    file_shares = []
    sku_name = "Standard_LRS"
    account_kind = "StorageV2"
    access_tier = "Hot"
    min_tls_version = "TLS1_2"
    allow_blob_public_access = false
  }
]

tags = {
  environment = "dev"
  owner       = "Chris House"
  project     = "ais-landing-zone"
}
