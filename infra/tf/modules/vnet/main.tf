resource "azurerm_virtual_network" "this" {
  name                = var.vnet_name
  location            = var.location
  resource_group_name = var.resource_group_name
  address_space       = var.address_spaces
  tags                = var.tags
}

resource "azurerm_subnet" "this" {
  for_each             = { for s in var.subnets : s.name => s }
  name                 = each.value.name
  resource_group_name  = var.resource_group_name
  virtual_network_name = azurerm_virtual_network.this.name
  address_prefixes     = each.value.address_prefixes

  dynamic "delegation" {
    for_each = try(each.value.delegation != null, false) ? [each.value.delegation] : []
    content {
      name = delegation.value.name
      service_delegation {
        name    = delegation.value.service_name
        actions = delegation.value.actions
      }
    }
  }

  service_endpoints = each.value.service_endpoints != null ? each.value.service_endpoints : null
}

resource "azurerm_network_security_group" "subnet_nsg" {
  for_each            = { for s in var.subnets : s.name => s if s.nsg != null }
  name                = each.value.nsg.name
  location            = var.location
  resource_group_name = var.resource_group_name

  dynamic "security_rule" {
    for_each = each.value.nsg.security_rules
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

resource "azurerm_subnet_network_security_group_association" "subnet_nsg_assoc" {
  for_each                  = { for s in var.subnets : s.name => s if s.nsg != null }
  subnet_id                 = azurerm_subnet.this[each.key].id
  network_security_group_id = azurerm_network_security_group.subnet_nsg[each.key].id
}

resource "azurerm_route_table" "subnet_rt" {
  for_each            = { for s in var.subnets : s.name => s if s.route_table != null }
  name                = each.value.route_table.name
  location            = var.location
  resource_group_name = var.resource_group_name
  route               = each.value.route_table.routes
}

resource "azurerm_subnet_route_table_association" "subnet_rt_assoc" {
  for_each       = { for s in var.subnets : s.name => s if s.route_table != null }
  subnet_id      = azurerm_subnet.this[each.key].id
  route_table_id = azurerm_route_table.subnet_rt[each.key].id
}

resource "azurerm_monitor_diagnostic_setting" "nsg" {
  for_each                   = { for s in var.subnets : s.name => s if s.nsg != null }
  name                       = "${each.value.nsg.name}-diag"
  target_resource_id         = azurerm_network_security_group.subnet_nsg[each.key].id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category_group = "AllLogs"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}
