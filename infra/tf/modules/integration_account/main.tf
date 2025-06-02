terraform {
  required_providers {
    azapi = {
      source = "Azure/azapi"
    }
    azurerm = {
      source = "hashicorp/azurerm"
    }
  }
}

# Data source to get current subscription
data "azurerm_subscription" "current" {}

# Logic App Integration Account using azapi for managed identity support
resource "azapi_resource" "this" {
  type      = "Microsoft.Logic/integrationAccounts@2019-05-01"
  name      = var.name
  location  = var.location
  parent_id = "/subscriptions/${data.azurerm_subscription.current.subscription_id}/resourceGroups/${var.resource_group_name}"

  body = jsonencode({
    sku = {
      name = var.config.sku_name
    }
    properties = {}
    identity = {
      type = "SystemAssigned"
    }
  })

  tags = var.tags
}

# Zone Redundant Storage Account for Premium SKU
resource "azurerm_storage_account" "integration_content" {
  count                    = var.config.sku_name == "Premium" ? 1 : 0
  name                     = replace("${var.name}ia", "-", "")
  resource_group_name      = var.resource_group_name
  location                 = var.location
  account_tier             = "Standard"
  account_replication_type = "ZRS"
  account_kind             = "StorageV2"
  min_tls_version          = "TLS1_2"

  # Disable public access for security
  public_network_access_enabled = false

  tags = var.tags
}

# Private DNS Zone for Integration Account (Premium SKU only)
module "integration_account_private_dns_zone" {
  count               = var.config.sku_name == "Premium" ? 1 : 0
  source              = "../private_dns_zone"
  zone_name           = "privatelink.logic.azure.com"
  resource_group_name = var.resource_group_name
  link_name           = "ia-dns-link"
  vnet_id             = var.vnet_id
  tags                = var.tags
}

# Private Endpoint for Integration Account (Premium SKU only)
module "integration_account_private_endpoint" {
  count                          = var.config.sku_name == "Premium" ? 1 : 0
  source                         = "../private_endpoint"
  name                           = "${var.name}-pe"
  location                       = var.location
  resource_group_name            = var.resource_group_name
  subnet_id                      = var.subnet_id
  connection_name                = "${var.name}-pe-conn"
  private_connection_resource_id = azapi_resource.this.id
  subresource_names              = ["integrationAccount"]
  private_dns_zone_ids           = var.config.sku_name == "Premium" ? [module.integration_account_private_dns_zone[0].zone_id] : []
  tags                           = var.tags
}

# Private DNS Zone for Storage Account (Premium SKU only)
module "storage_private_dns_zone" {
  count               = var.config.sku_name == "Premium" ? 1 : 0
  source              = "../private_dns_zone"
  zone_name           = "privatelink.blob.core.windows.net"
  resource_group_name = var.resource_group_name
  link_name           = "ia-storage-dns-link"
  vnet_id             = var.vnet_id
  tags                = var.tags
}

# Private Endpoint for Storage Account (Premium SKU only)
module "storage_private_endpoint" {
  count                          = var.config.sku_name == "Premium" ? 1 : 0
  source                         = "../private_endpoint"
  name                           = "${azurerm_storage_account.integration_content[0].name}-pe"
  location                       = var.location
  resource_group_name            = var.resource_group_name
  subnet_id                      = var.subnet_id
  connection_name                = "${azurerm_storage_account.integration_content[0].name}-pe-conn"
  private_connection_resource_id = azurerm_storage_account.integration_content[0].id
  subresource_names              = ["blob"]
  private_dns_zone_ids           = var.config.sku_name == "Premium" ? [module.storage_private_dns_zone[0].zone_id] : []
  tags                           = var.tags
}

# Diagnostic Settings for Integration Account
resource "azurerm_monitor_diagnostic_setting" "integration_account_diag" {
  name                       = "ia-diag"
  target_resource_id         = azapi_resource.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "IntegrationAccountTrackingEvents"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

# Diagnostic Settings for Storage Account (Premium SKU only)
resource "azurerm_monitor_diagnostic_setting" "storage_diag" {
  count                      = var.config.sku_name == "Premium" ? 1 : 0
  name                       = "storage-diag"
  target_resource_id         = "${azurerm_storage_account.integration_content[0].id}/blobServices/default"
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "StorageRead"
  }
  enabled_log {
    category = "StorageWrite"
  }
  enabled_log {
    category = "StorageDelete"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}