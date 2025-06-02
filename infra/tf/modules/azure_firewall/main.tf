resource "azurerm_public_ip" "this" {
  name                = "${var.config.name}-pip"
  location            = var.config.location
  resource_group_name = var.config.resource_group_name
  allocation_method   = "Static"
  sku                 = "Standard"
  tags                = var.config.tags
}

resource "azurerm_ip_group" "apim_subnet" {
  name                = "${var.config.name}-apim-subnet-ipgroup"
  location            = var.config.location
  resource_group_name = var.config.resource_group_name
  cidrs               = [var.config.apim_subnet_cidr]
  tags                = var.config.tags
}

resource "azurerm_firewall_policy" "this" {
  name                = "${var.config.name}-policy"
  resource_group_name = var.config.resource_group_name
  location            = var.config.location
  sku                 = var.config.sku_tier
  tags                = var.config.tags

}

resource "azurerm_firewall" "this" {
  name                = var.config.name
  location            = var.config.location
  resource_group_name = var.config.resource_group_name
  sku_name            = var.config.sku_name
  sku_tier            = var.config.sku_tier
  firewall_policy_id  = azurerm_firewall_policy.this.id
  tags                = var.config.tags

  ip_configuration {
    name                 = "ipconfig"
    subnet_id            = var.config.subnet_id
    public_ip_address_id = azurerm_public_ip.this.id
  }

  management_ip_configuration {
    name                 = "forcetunnel"
    subnet_id            = var.config.force_tunneling_subnet_id
    public_ip_address_id = azurerm_public_ip.forcetunnel.id
  }
}

resource "azurerm_public_ip" "forcetunnel" {
  name                = "${var.config.name}-forcetunnel-pip"
  location            = var.config.location
  resource_group_name = var.config.resource_group_name
  allocation_method   = "Static"
  sku                 = "Standard"
  tags                = var.config.tags
}

resource "azurerm_firewall_policy_rule_collection_group" "network_rules" {
  count              = length(var.config.network_rules) > 0 ? 1 : 0
  name               = "network-rules"
  firewall_policy_id = azurerm_firewall_policy.this.id
  priority           = 100

  network_rule_collection {
    name     = "network-rule-collection"
    priority = 100
    action   = "Allow"

    dynamic "rule" {
      for_each = var.config.network_rules
      content {
        name                  = rule.value.name
        description           = rule.value.description
        protocols             = rule.value.protocols
        source_addresses      = rule.value.source_addresses != null ? (length([for addr in rule.value.source_addresses : addr if addr != var.config.apim_subnet_cidr]) > 0 ? [for addr in rule.value.source_addresses : addr if addr != var.config.apim_subnet_cidr] : null) : null
        destination_addresses = rule.value.destination_addresses
        destination_ports     = rule.value.destination_ports
        source_ip_groups = concat(
          rule.value.source_ip_groups != null ? rule.value.source_ip_groups : [],
          rule.value.source_addresses != null && contains(rule.value.source_addresses, var.config.apim_subnet_cidr) ? [azurerm_ip_group.apim_subnet.id] : []
        )
        destination_ip_groups = rule.value.destination_ip_groups
      }
    }
  }
}

resource "azurerm_firewall_policy_rule_collection_group" "application_rules" {
  count              = length(var.config.application_rules) > 0 ? 1 : 0
  name               = "application-rules"
  firewall_policy_id = azurerm_firewall_policy.this.id
  priority           = 300

  application_rule_collection {
    name     = "application-rule-collection"
    priority = 100
    action   = "Allow"

    dynamic "rule" {
      for_each = var.config.application_rules
      content {
        name             = rule.value.name
        description      = rule.value.description
        source_addresses = rule.value.source_addresses != null ? (length([for addr in rule.value.source_addresses : addr if addr != var.config.apim_subnet_cidr]) > 0 ? [for addr in rule.value.source_addresses : addr if addr != var.config.apim_subnet_cidr] : null) : null
        source_ip_groups = concat(
          rule.value.source_ip_groups != null ? rule.value.source_ip_groups : [],
          rule.value.source_addresses != null && contains(rule.value.source_addresses, var.config.apim_subnet_cidr) ? [azurerm_ip_group.apim_subnet.id] : []
        )
        destination_fqdns = try(rule.value.destination_fqdns, null)
        dynamic "protocols" {
          for_each = rule.value.protocols != null ? rule.value.protocols : []
          content {
            port = protocols.value.port
            type = protocols.value.type
          }
        }
      }
    }
  }
}

resource "azurerm_firewall_policy_rule_collection_group" "nat_rules" {
  count              = var.config.enable_apim_dnat ? 1 : 0
  name               = "nat-rules"
  firewall_policy_id = azurerm_firewall_policy.this.id
  priority           = 200

  nat_rule_collection {
    name     = "nat-rule-collection"
    priority = 100
    action   = "Dnat"

    rule {
      name                = "InboundToAPIM"
      description         = "Inbound DNAT rule to APIM private interface"
      protocols           = ["TCP"]
      source_addresses    = ["*"]
      destination_address = azurerm_public_ip.this.ip_address
      destination_ports   = ["443"]
      translated_address  = var.config.apim_private_ip
      translated_port     = "443"
    }
  }
}

resource "azurerm_monitor_diagnostic_setting" "firewall" {
  name                           = "${var.config.name}-diag"
  target_resource_id             = azurerm_firewall.this.id
  log_analytics_workspace_id     = var.config.log_analytics_workspace_id
  log_analytics_destination_type = "Dedicated"

  enabled_log {
    category_group = "AllLogs"
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

resource "azurerm_monitor_diagnostic_setting" "public_ip" {
  name                       = "${var.config.name}-pip-diag"
  target_resource_id         = azurerm_public_ip.this.id
  log_analytics_workspace_id = var.config.log_analytics_workspace_id

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

resource "azurerm_monitor_diagnostic_setting" "public_ip_forcetunnel" {
  name                       = "${var.config.name}-forcetunnel-pip-diag"
  target_resource_id         = azurerm_public_ip.forcetunnel.id
  log_analytics_workspace_id = var.config.log_analytics_workspace_id

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