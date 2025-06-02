resource "azurerm_api_management" "this" {
  name                 = var.name
  location             = var.location
  resource_group_name  = var.resource_group_name
  publisher_name       = var.publisher_name
  publisher_email      = var.publisher_email
  sku_name             = "${var.sku_name}_${var.sku_capacity}"
  virtual_network_type = "Internal"
  virtual_network_configuration {
    subnet_id = var.subnet_id
  }

  dynamic "identity" {
    for_each = var.enable_system_assigned_identity || length(var.user_assigned_identity_ids) > 0 ? [1] : []
    content {
      type         = var.enable_system_assigned_identity && length(var.user_assigned_identity_ids) > 0 ? "SystemAssigned, UserAssigned" : var.enable_system_assigned_identity ? "SystemAssigned" : "UserAssigned"
      identity_ids = var.user_assigned_identity_ids
    }
  }

  tags = var.tags
}

resource "azurerm_monitor_diagnostic_setting" "apim" {
  name                           = "${var.name}-diag"
  target_resource_id             = azurerm_api_management.this.id
  log_analytics_workspace_id     = var.log_analytics_workspace_id
  log_analytics_destination_type = "Dedicated"
  enabled_log {
    category_group = "AllLogs"
  }

  enabled_log {
    category_group = "Audit"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}
