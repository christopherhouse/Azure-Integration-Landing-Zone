resource_group_name = "RG-AIS-LZ-TF"
location            = "eastus2"
subscription_id     = "c5d4a6e8-69bf-4148-be25-cb362f83c370"
suffix              = "lz-tf"
environment         = "dev"

spoke_vnet_address_spaces = [
  "10.10.0.0/16"
]

hub_vnet_address_spaces = [
  "192.168.100.0/23"
]

spoke_vnet_subnets = [
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
    name              = "private-endpoints"
    address_prefixes  = ["10.10.2.0/24"]
    nsg               = null
    route_table       = null
    delegation        = null
    service_endpoints = []
  },
  {
    name             = "apim"
    address_prefixes = ["10.10.3.0/24"]
    nsg = {
      name = "apim-nsg"
      security_rules = [
        // Inbound rules
        {
          name                       = "AllowApimManagementInbound"
          priority                   = 100
          direction                  = "Inbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "3443"
          source_address_prefix      = "ApiManagement"
          destination_address_prefix = "VirtualNetwork"
          description                = "Allow inbound management traffic from Azure API Management service on port 3443"
        },
        {
          name                       = "AllowAzureLoadBalancerInBound"
          priority                   = 200
          direction                  = "Inbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "6390"
          source_address_prefix      = "AzureLoadBalancer"
          destination_address_prefix = "VirtualNetwork"
          description                = "Allow inbound health probe traffic from Azure Load Balancer on port 6390"
        },
        {
          name                       = "AzureTrafficManagerInbound"
          priority                   = 300
          direction                  = "Inbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "443"
          source_address_prefix      = "AzureTrafficManager"
          destination_address_prefix = "Virtualnetwork"
          description                = "Allow inbound traffic from Azure Traffic Manager on port 443"
        },
        // Outbound rules
        {
          name                       = "AllowStorageOutbound"
          priority                   = 400
          direction                  = "Outbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "443"
          source_address_prefix      = "VirtualNetwork"
          destination_address_prefix = "Storage"
          description                = "Allow outbound traffic to Azure Storage on port 443"
        },
        {
          name                       = "AllowSqlOutbound"
          priority                   = 500
          direction                  = "Outbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "1433"
          source_address_prefix      = "VirtualNetwork"
          destination_address_prefix = "SQL"
          description                = "Allow outbound traffic to Azure SQL Database on port 1433"
        },
        {
          name                       = "AllowAzureKeyVaultOutbound"
          priority                   = 600
          direction                  = "Outbound"
          access                     = "Allow"
          protocol                   = "Tcp"
          source_port_range          = "*"
          destination_port_range     = "443"
          source_address_prefix      = "VirtualNetwork"
          destination_address_prefix = "AzureKeyVault"
          description                = "Allow outbound traffic to Azure Key Vault on port 443"
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
    route_table       = null
    delegation        = null
    service_endpoints = ["Microsoft.Storage", "Microsoft.Sql", "Microsoft.KeyVault", "Microsoft.EventHub", "Microsoft.ServiceBus"]
  }
]

hub_vnet_subnets = [
  {
    name              = "AzureFirewallSubnet"
    address_prefixes  = ["192.168.100.0/26"]
    nsg               = null
    route_table       = null
    delegation        = null
    service_endpoints = []
  },
  {
    name              = "AzureFirewallManagementSubnet"
    address_prefixes  = ["192.168.100.64/26"]
    nsg               = null
    route_table       = null
    delegation        = null
    service_endpoints = []
  }
]

key_vault_purge_protection_enabled   = true
key_vault_soft_delete_retention_days = 7

apim_publisher_name  = "Contoso"
apim_publisher_email = "apis@contoso.net"
apim_sku_name        = "Developer"
apim_sku_capacity    = 1

deploy_api_management          = true
deploy_app_service_environment = true

service_bus = {
  deploy = false
  capacity_units = 1
  queues = [
    {
      name                  = "orders-queue"
      max_size_in_megabytes = 1024
      default_message_ttl   = "P14D" # 14 days
      max_delivery_count    = 10
    },
    {
      name                                 = "notifications-queue"
      max_size_in_megabytes                = 1024
      default_message_ttl                  = "P7D" # 7 days
      max_delivery_count                   = 5
      requires_session                     = true
      dead_lettering_on_message_expiration = true
    }
  ]
  topics = [
    {
      name                  = "events"
      max_size_in_megabytes = 1024
      default_message_ttl   = "P14D" # 14 days
      subscriptions = [
        {
          name               = "all-events"
          max_delivery_count = 10
        },
        {
          name                = "critical-events"
          max_delivery_count  = 20
          default_message_ttl = "P7D" # 7 days
          requires_session    = true
        }
      ]
    },
    {
      name                  = "alerts"
      max_size_in_megabytes = 1024
      subscriptions = [
        {
          name               = "system-alerts"
          max_delivery_count = 10
        },
        {
          name                                      = "security-alerts"
          max_delivery_count                        = 10
          dead_lettering_on_message_expiration      = true
          dead_lettering_on_filter_evaluation_error = true
        }
      ]
    }
  ]
}

event_hub = {
  deploy   = true
  capacity = 1
  event_hubs = [
    {
      name              = "telemetry-events"
      partition_count   = 4
      message_retention = 7
      consumer_groups = [
        {
          name          = "analytics-processor"
          user_metadata = "Analytics team consumer group"
        },
        {
          name          = "alerts-processor"
          user_metadata = "Alerts processing consumer group"
        }
      ]
    },
    {
      name              = "audit-events"
      partition_count   = 2
      message_retention = 1
      consumer_groups = [
        {
          name          = "compliance-processor"
          user_metadata = "Compliance team consumer group"
        },
        {
          name          = "monitoring-processor"
          user_metadata = "Monitoring team consumer group"
        }
      ]
    }
  ]
}

deploy_azure_data_factory = false
# Example Data Factory configuration (uncomment to use)
# data_factory_public_network_enabled = false
data_factory_managed_private_endpoints = [
  {
    name               = "sql-server-endpoint"
    target_resource_id = "/subscriptions/{subscription_id}/resourceGroups/{resource_group}/providers/Microsoft.Sql/servers/{server_name}"
    subresource_name   = "sqlServer"
  },
  {
    name               = "storage-endpoint"
    target_resource_id = "/subscriptions/{subscription_id}/resourceGroups/{resource_group}/providers/Microsoft.Storage/storageAccounts/{storage_account_name}"
    subresource_name   = "blob"
  }
]

storage_accounts = [
  {
    name_prefix             = "apimbackup"
    private_endpoints       = ["blob"]
    create_private_dns_zone = true
    blob_containers = [
      {
        name          = "apimbackup"
        public_access = "None"
      }
    ]
    tables                   = []
    queues                   = []
    file_shares              = []
    sku_name                 = "Standard_LRS"
    account_kind             = "StorageV2"
    access_tier              = "Hot"
    min_tls_version          = "TLS1_2"
    allow_blob_public_access = false
  }
]

azure_firewall = {
  deploy_azure_firewall  = true
  sku_name               = "AZFW_VNet"
  sku_tier               = "Standard"
  network_rules = [
    {
      name                  = "AllowAzureMonitor"
      description           = "Allow traffic to Azure Monitor"
      priority              = 100
      action                = "Allow"
      source_addresses      = ["10.10.0.0/16"]
      destination_addresses = ["AzureMonitor"]
      destination_ports     = ["443", "12000", "1886"]
      protocols             = ["TCP"]
    },
    {
      name                  = "AllowApiManagementAPIs"
      description           = "Allow traffic to API Management APIs"
      priority              = 110
      action                = "Allow"
      source_addresses      = ["10.10.3.0/24"] # APIM subnet
      destination_addresses = ["ApiManagement"]
      destination_ports     = ["3443"]
      protocols             = ["TCP"]
    },
    {
      name                  = "AllowAzureActiveDirectory"
      description           = "Allow traffic to Azure Active Directory"
      priority              = 120
      action                = "Allow"
      source_addresses      = ["10.10.3.0/24"] # APIM subnet
      destination_addresses = ["AzureActiveDirectory"]
      destination_ports     = ["443"]
      protocols             = ["TCP"]
    },
    {
      name                  = "AllowAzureResourceManager"
      description           = "Allow traffic to Azure Resource Manager"
      priority              = 130
      action                = "Allow"
      source_addresses      = ["10.10.3.0/24"] # APIM subnet
      destination_addresses = ["AzureResourceManager"]
      destination_ports     = ["443"]
      protocols             = ["TCP"]
    }
  ]
  application_rules = [
    {
      name              = "AllowMicrosoftDocs"
      description       = "Allow traffic to Microsoft Docs"
      priority          = 100
      action            = "Allow"
      source_addresses  = ["10.10.0.0/16"]
      destination_fqdns = ["*.microsoft.com", "*.microsoftonline.com"]
      protocols = [
        {
          port = "443"
          type = "Https"
        },
        {
          port = "80"
          type = "Http"
        }
      ]
    },
    {
      name             = "AllowApiManagementRequiredServices"
      description      = "Allow traffic to Azure API Management required services"
      priority         = 110
      action           = "Allow"
      source_addresses = ["10.10.3.0/24"] # APIM subnet
      destination_fqdns = [
        "management.azure.com",
        "login.microsoftonline.com",
        "login.windows.net",
        "*.core.windows.net",
        "*.frontend.applicationinsights.azure.com",
        "*.monitoring.azure.com",
        "dc.services.visualstudio.com",
        "*.servicebus.windows.net",
        "*.events.data.microsoft.com",
        "global.metrics.nsatc.net",
        "shoebox2.events.data.microsoft.com"
      ]
      protocols = [
        {
          port = "443"
          type = "Https"
        }
      ]
    },
    {
      name             = "AllowApiManagementExtendedServices"
      description      = "Allow traffic to extended API Management services"
      priority         = 120
      action           = "Allow"
      source_addresses = ["10.10.3.0/24"] # APIM subnet
      destination_fqdns = [
        "whatismyipaddress.com", # For diagnostics 
        "*.azureedge.net",
        "*.azure-api.net",
        "*.cloudapp.net",
        "*.cloudapp.azure.com",
        "github.com",
        "api.github.com",
        "raw.githubusercontent.com"
      ]
      protocols = [
        {
          port = "443"
          type = "Https"
        }
      ]
    }
  ]
  nat_rules = [
    # {
    #   name                = "InboundToAPIM"
    #   description         = "Inbound NAT rule to APIM private interface"
    #   priority            = 100
    #   action              = "Dnat"
    #   source_addresses    = ["*"]
    #   destination_address = "" # Replace with actual public IP in production
    #   destination_ports   = ["443"]
    #   protocols           = ["TCP"]
    #   translated_address  = "10.10.3.4" # Replace with actual APIM private IP in production
    #   translated_port     = "443"
    # }
  ]
}

tags = {
  environment   = "dev"
  owner         = "Chris House"
  project       = "ais-landing-zone"
  "Cost Center" = "123DEF"
}
