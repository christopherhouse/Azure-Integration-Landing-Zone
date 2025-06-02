resource "azurerm_network_security_group" "this" {
  name                = var.name
  location            = var.location
  resource_group_name = var.resource_group_name
  tags                = var.tags
  dynamic "security_rule" {
    for_each = var.security_rules
    content {
      name                         = security_rule.value.name
      priority                     = security_rule.value.priority
      direction                    = security_rule.value.direction
      access                       = security_rule.value.access
      protocol                     = security_rule.value.protocol
      source_port_range            = try(security_rule.value.source_port_range, null)
      source_port_ranges           = try(security_rule.value.source_port_ranges, null)
      destination_port_range       = try(security_rule.value.destination_port_range, null)
      destination_port_ranges      = try(security_rule.value.destination_port_ranges, null)
      source_address_prefix        = try(security_rule.value.source_address_prefix, null)
      source_address_prefixes      = try(security_rule.value.source_address_prefixes, null)
      destination_address_prefix   = try(security_rule.value.destination_address_prefix, null)
      destination_address_prefixes = try(security_rule.value.destination_address_prefixes, null)
      description                  = security_rule.value.description
    }
  }
}

resource "azurerm_monitor_diagnostic_setting" "nsg" {
  name                       = "${var.name}-diag"
  target_resource_id         = azurerm_network_security_group.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category_group = "AllLogs"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}