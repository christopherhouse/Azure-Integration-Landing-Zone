# 🚀 Azure Integration Landing Zone

Welcome to the **Azure Integration Landing Zone**!  
This repository provides a modular, production-ready Terraform codebase for deploying a secure, scalable Azure integration environment.

---

## 📦 What’s Inside?

- **Modular Terraform Code**:  
  - Resource Group
  - Log Analytics Workspace
  - Virtual Network (with flexible subnet, NSG, route table, and delegation support)
  - Azure Key Vault (with RBAC, diagnostics, and security best practices)
  - Azure API Management (internal VNet integration, diagnostics, identity, and logging)
  - Azure Service Bus (Premium tier, private endpoint, and availability zone support)
  - Azure Firewall (Standard SKU, force tunneling, network/application/NAT rules)

  - Azure Data Factory (managed virtual network, private endpoints, and secure connectivity)
  - Azure Naming module integration for consistent resource names

- **Best Practices**:  
  - Azure RBAC
  - Diagnostics routed to Log Analytics
  - Soft delete & purge protection for Key Vault
  - Public access disabled where appropriate

---

## 🛠️ How to Use

### 1. Clone the Repo

```sh
git clone https://github.com/your-org/azure-integration-landing-zone.git
cd azure-integration-landing-zone/infra/tf
```

### 2. Configure Your `terraform.tfvars`

Example:

```hcl
resource_group_name = "RG-AIS-LZ-TF"
location            = "eastus2"
log_analytics_workspace_name = "log-ais-lz-tf"
subscription_id = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
suffix       = "lz"
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
  }
]

key_vault_purge_protection_enabled   = true
key_vault_soft_delete_retention_days = 7

# ---
# API Management (APIM) module configuration (optional)
# Remove or comment out this section if you do not wish to deploy APIM
# ---
apim_enabled = true
apim_name = "apimlz-tfdevoluo"
apim_sku_name = "Developer"
apim_sku_capacity = 1
apim_publisher_name = "Your Company"
apim_publisher_email = "admin@yourcompany.com"
apim_subnet_id = "/subscriptions/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/resourceGroups/RG-AIS-LZ-TF/providers/Microsoft.Network/virtualNetworks/vnet-ais-lz-tf/subnets/apim"
apim_enable_system_assigned_identity = true
apim_user_assigned_identity_ids = []

# ---
# Service Bus module configuration (optional)
# Remove or comment out this section if you do not wish to deploy Service Bus
# ---
deploy_service_bus = true
service_bus_capacity_units = 1
```

> **Note:** The API Management (APIM) module is optional. If you do not wish to deploy APIM, set `deploy_api_management = false` or remove the APIM-related variables from your `terraform.tfvars`.

> **Note:** The Service Bus module is optional. If you do not wish to deploy Service Bus, set `deploy_service_bus = false` or remove the Service Bus-related variables from your `terraform.tfvars`.

### 3. Initialize & Deploy

```sh
terraform init
terraform plan -out tfplan
terraform apply tfplan
```

---

## 📚 Structure

```
infra/
  tf/
    main.tf
    variables.tf
    terraform.tfvars
    modules/
      names/
      log_analytics/
      vnet/
      key_vault/
      api_management/   # Azure API Management module (optional)
      service_bus/      # Azure Service Bus module (optional)
      data_factory/     # Azure Data Factory module (optional)
```

---

## 🔹 API Management Module

> **This module is optional.**

The **API Management** module provisions an Azure API Management instance with:
- Internal VNet integration for secure, private access
- System-assigned and/or user-assigned managed identity support
- Diagnostic settings routed to Log Analytics (audit, gateway, websocket logs, metrics)
- Flexible SKU and capacity configuration
- Best practices for security and monitoring

This module is ideal for organizations needing centralized API gateway capabilities with enterprise-grade security and observability.

---

## 🔹 Service Bus Module

> **This module is optional.**

The **Service Bus** module provisions an Azure Service Bus namespace with:
- Premium SKU for enterprise-grade messaging capabilities
- Private endpoint integration for secure, private access
- Configurable capacity units (1, 2, 4, 8, 16)
- Diagnostic settings routed to Log Analytics
- Private DNS zone integration
- Queues with configurable properties (size, TTL, delivery count, sessions, dead-lettering)
- Topics with subscriptions (various subscription properties: TTL, locks, sessions, dead-lettering)

The module includes example queue and topic configurations out of the box. These can be customized or extended based on your specific needs.

### Features

- Creates an Azure Service Bus namespace with Premium SKU
- Provisions queues with configurable settings (e.g., max size, TTL, partitioning)
- Provisions topics with configurable settings
- Provisions subscriptions for topics with configurable properties
- Integrates with private networking via private endpoints
- Configures diagnostic settings for monitoring and logging

### Usage

You can enable the Service Bus module in your terraform.tfvars:

```hcl
deploy_service_bus = true
service_bus_capacity_units = 1
```

---

## 🔹 Event Hub Module

> **This module is optional.**

The **Event Hub** module provisions an Azure Event Hub namespace with:
- Standard SKU for reliable event streaming capabilities
- Private endpoint integration for secure, private access
- Configurable capacity units (1, 2, 4, 8, 12, 20, 40)
- Diagnostic settings routed to Log Analytics
- Private DNS zone integration
- Event hubs with configurable partition count and message retention
- Consumer groups for event processing with metadata support

The module includes example event hub and consumer group configurations out of the box. These can be customized or extended based on your specific event streaming needs.

### Features

- Creates an Azure Event Hub namespace with Standard SKU
- Provisions event hubs with configurable settings (partition count, message retention)
- Provisions consumer groups for event hubs with optional metadata
- Integrates with private networking via private endpoints
- Configures diagnostic settings for monitoring and logging
- Disables public network access for enhanced security

### Usage

You can enable the Event Hub module in your terraform.tfvars:

```hcl
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
    }
  ]
}
```

### Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| deploy | Controls whether the Event Hub module is deployed | `bool` | `false` | no |
| capacity | Specifies the capacity units for the Event Hub namespace. Valid values are 1, 2, 4, 8, 12, 20, or 40. | `number` | `1` | no |
| event_hubs | List of Event Hub configurations to create | `list(object)` | `[]` | no |

### Outputs

| Name | Description |
|------|-------------|
| namespace_id | The ID of the Event Hub namespace |
| namespace_name | The name of the Event Hub namespace |
| primary_connection_string | The primary connection string for the Event Hub namespace |
| event_hub_ids | Map of event hub names to their resource IDs |
| consumer_group_ids | Map of consumer group identifiers to their resource IDs |

---

## 🔹 Data Factory Module

> **This module is optional.**

The **Data Factory** module provisions an Azure Data Factory instance with:
- Managed virtual network for secure data integration
- Private endpoint integration for secure, private access
- Managed private endpoints for connecting to data sources securely
- System and/or user-assigned managed identity support
- Diagnostic settings routed to Log Analytics
- Git integration for CI/CD workflows

This module is ideal for organizations requiring secure data integration and ETL/ELT processes with enterprise-grade security and governance.

### Features

- Enables managed virtual network for secure data integration
- Supports creation of managed private endpoints for connecting to data sources securely
- Configures private endpoints for secure access to the Data Factory itself
- Integrates with Azure RBAC through configurable managed identities
- Configures diagnostic settings for comprehensive monitoring and logging
- Supports Git integration for CI/CD pipelines
- Implements networking security best practices by default

### Usage

You can enable the Data Factory module in your terraform.tfvars:

```hcl
deploy_data_factory = true
data_factory_public_network_enabled = false  # Disable public network access for security

# Configure managed private endpoints to connect to your data sources
data_factory_managed_private_endpoints = [
  {
    name = "sql-server-endpoint"
    target_resource_id = "/subscriptions/{subscription_id}/resourceGroups/{resource_group}/providers/Microsoft.Sql/servers/{server_name}"
    subresource_name = "sqlServer"
  },
  {
    name = "storage-endpoint"
    target_resource_id = "/subscriptions/{subscription_id}/resourceGroups/{resource_group}/providers/Microsoft.Storage/storageAccounts/{storage_account_name}"
    subresource_name = "blob"
  }
]
```

Then, you can either use the default examples or customize queue and topic configurations:

```hcl
module "service_bus" {
  source              = "./modules/service_bus"
  name                = "sb-integration-dev"
  location            = "eastus2"
  resource_group_name = "rg-integration-dev"
  capacity_units      = 1
  log_analytics_workspace_id = module.log_analytics.workspace_id
  subnet_id           = module.vnet.subnet_ids["private-endpoints"]
  vnet_id             = module.vnet.vnet_id
  
  # Define custom queues
  queues = [
    {
      name                  = "orders-queue"
      max_size_in_megabytes = 1024
      default_message_ttl   = "P14D"  # 14 days
      max_delivery_count    = 10
    },
    {
      name                              = "notifications-queue"
      max_size_in_megabytes             = 1024
      default_message_ttl               = "P7D"   # 7 days
      max_delivery_count                = 5
      requires_session                  = true
      dead_lettering_on_message_expiration = true
    }
  ]
  
  # Define custom topics with subscriptions
  topics = [
    {
      name                  = "events"
      max_size_in_megabytes = 1024
      default_message_ttl   = "P14D"  # 14 days
      subscriptions = [
        {
          name              = "all-events"
          max_delivery_count = 10
        },
        {
          name                = "critical-events"
          max_delivery_count  = 20
          default_message_ttl = "P7D"  # 7 days
          requires_session    = true
        }
      ]
    }
  ]
  
  tags = {
    Environment = "Development"
  }
}
```

### Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| name | The name of the Service Bus namespace | `string` | n/a | yes |
| location | The Azure region where resources should be created | `string` | n/a | yes |
| resource_group_name | The name of the resource group | `string` | n/a | yes |
| capacity_units | Specifies the capacity units for the Service Bus namespace (Premium tier only). Valid values are 1, 2, 4, 8, or 16. | `number` | n/a | yes |
| log_analytics_workspace_id | The ID of the Log Analytics workspace to which diagnostic logs will be sent | `string` | n/a | yes |
| subnet_id | The ID of the subnet for the private endpoint | `string` | n/a | yes |
| vnet_id | The ID of the virtual network for the private DNS zone link | `string` | n/a | yes |
| queues | List of Service Bus queues to create | `list(object)` | `[]` | no |
| topics | List of Service Bus topics to create | `list(object)` | `[]` | no |
| tags | A mapping of tags to assign to the resources | `map(string)` | `{}` | no |

### Outputs

| Name | Description |
|------|-------------|
| namespace_id | The ID of the Service Bus namespace |
| namespace_name | The name of the Service Bus namespace |
| primary_connection_string | The primary connection string for the Service Bus namespace |
| queue_ids | Map of queue names to their resource IDs |
| topic_ids | Map of topic names to their resource IDs |
| subscription_ids | Map of subscription identifiers to their resource IDs |

### Example Implementation

A complete example implementation is available in the [examples/service_bus](/infra/tf/examples/service_bus) directory, which demonstrates:

- Creating custom queues with various configurations:
  - Size limits (1GB to 5GB)
  - Message TTL (1 day to 30 days)
  - Session support
  - Duplicate detection
  - Dead-lettering
  
- Creating custom topics with subscriptions:
  - Multiple subscription types
  - Filtering capabilities
  - Session support
  - Dead-lettering on expiration

This module is ideal for organizations requiring a robust messaging infrastructure with enterprise-grade security, reliability, and scalability.

---

## 🔹 Azure Firewall Module

> **This module is optional.**

The **Azure Firewall** module provisions an Azure Firewall with:
- Standard SKU with built-in high availability
- Force tunneling for controlling outbound traffic
- Flexible rule configuration for network, application, and NAT rules
- Complete integration with Log Analytics for monitoring

### Features

- Creates an Azure Firewall with Standard SKU
- Enables force tunneling for controlling outbound traffic
- Configures network rules for filtering traffic based on source, destination, port, and protocol
- Configures application rules for filtering outbound HTTP/S traffic based on FQDNs
- Configures NAT rules for translating and filtering inbound traffic
- Integrates with Log Analytics for comprehensive monitoring and logging

### Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------| 
| name | The name of the Azure Firewall | `string` | n/a | yes |
| location | The Azure region where resources should be created | `string` | n/a | yes |
| resource_group_name | The name of the resource group | `string` | n/a | yes |
| sku_tier | The SKU tier of the firewall (Standard or Premium) | `string` | `"Standard"` | no |
| subnet_id | The ID of the subnet for Azure Firewall | `string` | n/a | yes |
| enable_force_tunneling | Enable force tunneling for Azure Firewall | `bool` | `true` | no |
| force_tunneling_subnet_id | The ID of the subnet for Azure Firewall's force tunneling | `string` | n/a | yes |
| log_analytics_workspace_id | The ID of the Log Analytics workspace for diagnostic logs | `string` | n/a | yes |
| network_rules | List of network rules to apply to the firewall | `list(object)` | `[]` | no |
| application_rules | List of application rules to apply to the firewall | `list(object)` | `[]` | no |
| nat_rules | List of NAT rules to apply to the firewall | `list(object)` | `[]` | no |

### Example Implementation

```hcl
module "azure_firewall" {
  source                    = "./modules/azure_firewall"
  name                      = module.names.firewall_name
  location                  = var.location
  resource_group_name       = var.resource_group_name
  subnet_id                 = module.vnet.subnet_ids["AzureFirewallSubnet"]
  enable_force_tunneling    = true
  force_tunneling_subnet_id = module.vnet.subnet_ids["AzureFirewallManagementSubnet"]
  log_analytics_workspace_id = module.log_analytics.workspace_id
  
  # Network rules for outbound traffic
  network_rules = [
    {
      name                  = "AllowAzureMonitor"
      description           = "Allow traffic to Azure Monitor"
      priority              = 100
      action                = "Allow"
      source_addresses      = ["10.10.0.0/16"]
      destination_addresses = ["AzureMonitor"]
      destination_ports     = ["443"]
      protocols             = ["TCP"]
    }
  ]
  
  # Application rules for web traffic
  application_rules = [
    {
      name             = "AllowMicrosoftDocs"
      description      = "Allow traffic to Microsoft Docs"
      priority         = 100
      action           = "Allow"
      source_addresses = ["10.10.0.0/16"]
      target_fqdns     = ["*.microsoft.com"]
      protocols = [
        {
          port = "443"
          type = "Https"
        }
      ]
    }
  ]
  
  # NAT rules for inbound traffic
  nat_rules = [
    {
      name                = "InboundToAPIM"
      description         = "Inbound NAT rule to APIM private interface"
      priority            = 100
      action              = "Dnat"
      source_addresses    = ["*"]
      destination_address = azurerm_public_ip.example.ip_address
      destination_ports   = ["443"]
      protocols           = ["TCP"]
      translated_address  = module.apim.private_ip_address
      translated_port     = "443"
    }
  ]
}
```

---

## 💡 Tips

- All resource names are generated using the [Azure Naming Terraform Module](https://registry.terraform.io/modules/Azure/naming/azurerm/latest).
- Sensitive files like `*.tfvars` and state files are gitignored.
- Use Azure CLI authentication for best experience:  
  `az login` before running Terraform.

---

## 📝 License

MIT

---

<p align="center">
  <img src="https://img.shields.io/badge/Azure-Integration-blue?logo=microsoftazure" />
  <img src="https://img.shields.io/badge/Terraform-Ready-623CE4?logo=terraform" />
</p>
