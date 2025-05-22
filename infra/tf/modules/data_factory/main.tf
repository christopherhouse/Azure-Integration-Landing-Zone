resource "azurerm_data_factory" "this" {
  name                            = var.name
  location                        = var.location
  resource_group_name             = var.resource_group_name
  public_network_enabled          = var.public_network_enabled
  managed_virtual_network_enabled = var.enable_managed_virtual_network

  dynamic "github_configuration" {
    for_each = var.git_configuration != null ? [var.git_configuration] : []
    content {
      account_name    = github_configuration.value.account_name
      repository_name = github_configuration.value.repository_name
      branch_name     = github_configuration.value.branch_name
      root_folder     = github_configuration.value.root_folder
      git_url         = "https://github.com"
    }
  }

  dynamic "identity" {
    for_each = var.identity_type != null ? [1] : []
    content {
      type         = var.identity_type
      identity_ids = var.identity_type == "UserAssigned" || var.identity_type == "SystemAssigned,UserAssigned" ? var.user_assigned_identity_ids : null
    }
  }

  tags = var.tags
}

# Only create managed private endpoints if managed VNet is enabled
resource "azurerm_data_factory_managed_private_endpoint" "this" {
  for_each           = { for endpoint in var.managed_private_endpoints : endpoint.name => endpoint if var.enable_managed_virtual_network }
  name               = each.value.name
  data_factory_id    = azurerm_data_factory.this.id
  target_resource_id = each.value.target_resource_id
  subresource_name   = each.value.subresource_name
  fqdns              = each.value.fqdns
}

resource "azurerm_monitor_diagnostic_setting" "data_factory_diag" {
  name                       = "adf-diag"
  target_resource_id         = azurerm_data_factory.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "ActivityRuns"
  }

  enabled_log {
    category = "PipelineRuns"
  }

  enabled_log {
    category = "TriggerRuns"
  }

  enabled_log {
    category = "SSISIntegrationRuntimeLogs"
  }

  enabled_log {
    category = "SSISPackageEventMessageContext"
  }

  enabled_log {
    category = "SSISPackageEventMessages"
  }

  enabled_log {
    category = "SSISPackageExecutableStatistics"
  }

  enabled_log {
    category = "SSISPackageExecutionComponentPhases"
  }

  enabled_log {
    category = "SSISPackageExecutionDataStatistics"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

module "private_dns_zone" {
  source              = "../private_dns_zone"
  zone_name           = "privatelink.datafactory.azure.net"
  resource_group_name = var.resource_group_name
  link_name           = "adf-dns-link"
  vnet_id             = var.vnet_id
  tags                = var.tags
}

module "private_endpoint" {
  source                         = "../private_endpoint"
  name                           = "${var.name}-pe"
  location                       = var.location
  resource_group_name            = var.resource_group_name
  subnet_id                      = var.subnet_id
  connection_name                = "${var.name}-pe-conn"
  private_connection_resource_id = azurerm_data_factory.this.id
  subresource_names              = ["dataFactory"]
  tags                           = var.tags
}