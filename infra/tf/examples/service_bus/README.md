# Azure Service Bus Module Example

This directory contains an example of how to use the Azure Service Bus module.

## main.tf
```hcl
provider "azurerm" {
  features {}
}

resource "azurerm_resource_group" "example" {
  name     = "rg-service-bus-example"
  location = "eastus2"
}

module "log_analytics" {
  source              = "../../modules/log_analytics"
  workspace_name      = "log-sb-example"
  location            = azurerm_resource_group.example.location
  resource_group_name = azurerm_resource_group.example.name
  tags                = local.tags
}

module "vnet" {
  source              = "../../modules/vnet"
  name                = "vnet-sb-example"
  location            = azurerm_resource_group.example.location
  resource_group_name = azurerm_resource_group.example.name
  address_spaces      = ["10.0.0.0/16"]

  subnets = [
    {
      name             = "private-endpoints"
      address_prefixes = ["10.0.1.0/24"]
      route_table      = null
      delegation       = null
      service_endpoints = [
        "Microsoft.ServiceBus"
      ]
    }
  ]

  tags = local.tags
}

# Deploy Service Bus with custom queues and topics
module "service_bus" {
  source                     = "../../modules/service_bus"
  name                       = "sb-custom-example"
  location                   = azurerm_resource_group.example.location
  resource_group_name        = azurerm_resource_group.example.name
  capacity_units             = 1
  log_analytics_workspace_id = module.log_analytics.workspace_id
  subnet_id                  = module.vnet.subnet_ids["private-endpoints"]
  vnet_id                    = module.vnet.vnet_id
  
  # Custom queues configuration
  queues = [
    {
      name                              = "custom-orders"
      max_size_in_megabytes             = 2048                  # 2 GB
      default_message_ttl               = "P30D"                # 30 days
      max_delivery_count                = 15
      requires_duplicate_detection      = true
      requires_session                  = false
      dead_lettering_on_message_expiration = true
    },
    {
      name                              = "high-priority"
      max_size_in_megabytes             = 5120                  # 5 GB
      default_message_ttl               = "P1D"                 # 1 day
      max_delivery_count                = 20
      lock_duration                     = "PT5M"                # 5 minutes
      requires_session                  = true
    }
  ]
  
  # Custom topics configuration
  topics = [
    {
      name                         = "custom-events"
      max_size_in_megabytes        = 5120                       # 5 GB
      default_message_ttl          = "P7D"                      # 7 days
      requires_duplicate_detection = true
      support_ordering             = true
      subscriptions = [
        {
          name                            = "all-custom-events"
          max_delivery_count              = 10
          requires_session                = false
        },
        {
          name                            = "filtered-events"
          max_delivery_count              = 5
          default_message_ttl             = "P3D"               # 3 days
          dead_lettering_on_message_expiration = true
        }
      ]
    },
    {
      name                  = "transactions"
      max_size_in_megabytes = 1024                              # 1 GB
      subscriptions = [
        {
          name                = "payment-transactions"
          max_delivery_count  = 10
          lock_duration       = "PT2M"                          # 2 minutes
          requires_session    = true
        },
        {
          name                = "error-transactions"
          max_delivery_count  = 5
          dead_lettering_on_message_expiration = true
        }
      ]
    }
  ]
  
  tags = local.tags
}

locals {
  tags = {
    Environment = "Example"
    Terraform   = "true"
    Project     = "Service Bus Demo"
  }
}
```

## terraform.tfvars
```hcl
# No tfvars needed as all values are hardcoded in the example
```

## outputs.tf
```hcl
output "service_bus_namespace_name" {
  description = "The name of the Service Bus namespace"
  value       = module.service_bus.namespace_name
}

output "service_bus_namespace_id" {
  description = "The ID of the Service Bus namespace"
  value       = module.service_bus.namespace_id
}

output "service_bus_queues" {
  description = "The IDs of the Service Bus queues"
  value       = module.service_bus.queue_ids
}

output "service_bus_topics" {
  description = "The IDs of the Service Bus topics"
  value       = module.service_bus.topic_ids
}

output "service_bus_subscriptions" {
  description = "The IDs of the Service Bus subscriptions"
  value       = module.service_bus.subscription_ids
}
```

## How to Use This Example

1. Navigate to this directory:
   ```bash
   cd infra/tf/examples/service_bus
   ```

2. Initialize Terraform:
   ```bash
   terraform init
   ```

3. Plan the changes:
   ```bash
   terraform plan -out tfplan
   ```

4. Apply the changes:
   ```bash
   terraform apply tfplan
   ```

5. When finished, destroy the infrastructure:
   ```bash
   terraform destroy
   ```