resource "azurerm_servicebus_namespace" "this" {
  name                = var.name
  location            = var.location
  resource_group_name = var.resource_group_name
  sku                 = "Premium" # Always Premium SKU as per requirement
  capacity            = var.capacity_units
  zone_redundant      = length(var.availability_zones) > 0

  tags = var.tags
}

resource "azurerm_monitor_diagnostic_setting" "servicebus_diag" {
  name                       = "sb-diag"
  target_resource_id         = azurerm_servicebus_namespace.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "OperationalLogs"
  }

  enabled_log {
    category = "VNetAndIPFilteringLogs"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

module "private_dns_zone" {
  source              = "../private_dns_zone"
  zone_name           = "privatelink.servicebus.windows.net"
  resource_group_name = var.resource_group_name
  link_name           = "sb-dns-link"
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
  private_connection_resource_id = azurerm_servicebus_namespace.this.id
  subresource_names              = ["namespace"]
  tags                           = var.tags
}