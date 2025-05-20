# Azure Service Bus Module

This module deploys an Azure Service Bus namespace with queues, topics, and subscriptions.

## Features

- Creates an Azure Service Bus namespace with Premium SKU
- Provisions queues with configurable settings (e.g., max size, TTL, partitioning)
- Provisions topics with configurable settings
- Provisions subscriptions for topics with configurable properties
- Integrates with private networking via private endpoints
- Configures diagnostic settings for monitoring and logging

## Usage

```hcl
module "service_bus" {
  source              = "./modules/service_bus"
  name                = "sb-integration-dev"  # Or use your naming module
  location            = "eastus2"
  resource_group_name = "rg-integration-dev"
  capacity_units      = 1
  log_analytics_workspace_id = "/subscriptions/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/resourceGroups/rg-integration-dev/providers/Microsoft.OperationalInsights/workspaces/log-integration-dev"
  subnet_id           = "/subscriptions/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/resourceGroups/rg-integration-dev/providers/Microsoft.Network/virtualNetworks/vnet-integration-dev/subnets/private-endpoints"
  vnet_id             = "/subscriptions/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/resourceGroups/rg-integration-dev/providers/Microsoft.Network/virtualNetworks/vnet-integration-dev"
  
  # Define queues
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
  
  # Define topics with subscriptions
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
    },
    {
      name                  = "alerts"
      max_size_in_megabytes = 1024
      subscriptions = [
        {
          name                = "system-alerts"
          max_delivery_count  = 10
        },
        {
          name                                   = "security-alerts"
          max_delivery_count                     = 10
          dead_lettering_on_message_expiration   = true
          dead_lettering_on_filter_evaluation_error = true
        }
      ]
    }
  ]
  
  tags = {
    Environment = "Development"
    CostCenter  = "IT"
  }
}
```

## Optional Integration with Parent Module

This module can be enabled/disabled with the `deploy_service_bus` variable in the parent module:

```hcl
module "service_bus" {
  count               = var.deploy_service_bus ? 1 : 0
  source              = "./modules/service_bus"
  # ... other parameters
}
```

## Inputs

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

## Outputs

| Name | Description |
|------|-------------|
| namespace_id | The ID of the Service Bus namespace |
| namespace_name | The name of the Service Bus namespace |
| primary_connection_string | The primary connection string for the Service Bus namespace |
| queue_ids | Map of queue names to their resource IDs |
| topic_ids | Map of topic names to their resource IDs |
| subscription_ids | Map of subscription identifiers to their resource IDs |