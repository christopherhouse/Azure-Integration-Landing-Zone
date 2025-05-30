resource "azurerm_public_ip" "this" {
  name                = "${var.name}-pip"
  location            = var.location
  resource_group_name = var.resource_group_name
  allocation_method   = "Static"
  sku                 = "Standard"
  tags                = var.tags
}

resource "azurerm_firewall_policy" "this" {
  name                = "${var.name}-policy"
  resource_group_name = var.resource_group_name
  location            = var.location
  sku                 = var.sku_tier
  tags                = var.tags
}

resource "azurerm_firewall" "this" {
  name                = var.name
  location            = var.location
  resource_group_name = var.resource_group_name
  sku_name            = var.sku_name
  sku_tier            = var.sku_tier
  firewall_policy_id  = azurerm_firewall_policy.this.id
  tags                = var.tags

  ip_configuration {
    name                 = "ipconfig"
    subnet_id            = var.subnet_id
    public_ip_address_id = azurerm_public_ip.this.id
  }

  management_ip_configuration {
    name                 = "forcetunnel"
    subnet_id            = var.force_tunneling_subnet_id
    public_ip_address_id = azurerm_public_ip.forcetunnel.id
  }
}

resource "azurerm_public_ip" "forcetunnel" {
  name                = "${var.name}-forcetunnel-pip"
  location            = var.location
  resource_group_name = var.resource_group_name
  allocation_method   = "Static"
  sku                 = "Standard"
  tags                = var.tags
}

resource "azurerm_firewall_policy_rule_collection_group" "network_rules" {
  count              = length(var.network_rules) > 0 ? 1 : 0
  name               = "network-rules"
  firewall_policy_id = azurerm_firewall_policy.this.id
  priority           = 100

  network_rule_collection {
    name     = "network-rule-collection"
    priority = 100
    action   = "Allow"

    dynamic "rule" {
      for_each = var.network_rules
      content {
        name                  = rule.value.name
        description           = rule.value.description
        protocols             = rule.value.protocols
        source_addresses      = rule.value.source_addresses
        destination_addresses = rule.value.destination_addresses
        destination_ports     = rule.value.destination_ports
        source_ip_groups      = rule.value.source_ip_groups
        destination_ip_groups = rule.value.destination_ip_groups
      }
    }
  }
}

resource "azurerm_firewall_policy_rule_collection_group" "application_rules" {
  count              = length(var.application_rules) > 0 ? 1 : 0
  name               = "application-rules"
  firewall_policy_id = azurerm_firewall_policy.this.id
  priority           = 300

  application_rule_collection {
    name     = "application-rule-collection"
    priority = 100
    action   = "Allow"

    dynamic "rule" {
      for_each = var.application_rules
      content {
        name             = rule.value.name
        description      = rule.value.description
        source_addresses = rule.value.source_addresses
        source_ip_groups = rule.value.source_ip_groups
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
  count              = length(var.nat_rules) > 0 ? 1 : 0
  name               = "nat-rules"
  firewall_policy_id = azurerm_firewall_policy.this.id
  priority           = 200

  nat_rule_collection {
    name     = "nat-rule-collection"
    priority = 100
    action   = "Dnat"

    dynamic "rule" {
      for_each = var.nat_rules
      content {
        name                = rule.value.name
        description         = rule.value.description
        protocols           = rule.value.protocols
        source_addresses    = rule.value.source_addresses
        destination_address = rule.value.destination_address
        destination_ports   = rule.value.destination_ports
        source_ip_groups    = rule.value.source_ip_groups
        translated_address  = rule.value.translated_address
        translated_port     = rule.value.translated_port
      }
    }
  }
}

resource "azurerm_monitor_diagnostic_setting" "firewall" {
  name                       = "${var.name}-diag"
  target_resource_id         = azurerm_firewall.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  dynamic "enabled_log" {
    for_each = [
      "AzureFirewallApplicationRule",
      "AzureFirewallNetworkRule",
      "AzureFirewallDnsProxy",
      "AZFWApplicationRule",
      "AZFWDnsQuery",
      "AZFWFatFlow",
      "AZFWFlowTrace",
      "AZFWFqdnResolveFailure",
      "AZFWIdpsSignature",
      "AZFWNatRule",
      "AZFWNetworkRule",
      "AZFWThreatIntel"
    ]
    content {
      category = enabled_log.value
    }
  }

  metric {
    category = "AllMetrics"
    enabled  = true
  }
}