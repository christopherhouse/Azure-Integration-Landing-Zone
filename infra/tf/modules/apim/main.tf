variable "name" { type = string }
variable "resource_group_name" { type = string }
variable "location" { type = string }
variable "log_analytics_workspace_id" { type = string }
variable "subnet_id" { type = string }
variable "sku_name" { type = string }
variable "enable_system_assigned_identity" { type = bool }
variable "user_assigned_identity_ids" { type = list(string) }

resource "azurerm_api_management" "this" {
  name                = var.name
  location            = var.location
  resource_group_name = var.resource_group_name
  publisher_name      = "admin"
  publisher_email     = "admin@example.com"
  sku_name            = var.sku_name
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
}

resource "azurerm_monitor_diagnostic_setting" "apim" {
  name                       = "${var.name}-diag"
  target_resource_id         = azurerm_api_management.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  dynamic "log" {
    for_each = [
      "GatewayLogs"
    ]
    content {
      category = log.value
      enabled  = true
    }
  }

  dynamic "metric" {
    for_each = ["AllMetrics"]
    content {
      category = metric.value
      enabled  = true
    }
  }
}
