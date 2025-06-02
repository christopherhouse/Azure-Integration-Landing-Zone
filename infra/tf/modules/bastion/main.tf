resource "azurerm_public_ip" "bastion" {
  name                = "${var.config.name}-pip"
  location            = var.config.location
  resource_group_name = var.config.resource_group_name
  allocation_method   = "Static"
  sku                 = "Standard"
  tags                = var.config.tags
}

resource "azurerm_bastion_host" "bastion" {
  name                   = var.config.name
  location               = var.config.location
  resource_group_name    = var.config.resource_group_name
  sku                    = var.config.sku
  copy_paste_enabled     = var.config.copy_paste_enabled
  file_copy_enabled      = var.config.file_copy_enabled
  ip_connect_enabled     = var.config.ip_connect_enabled
  shareable_link_enabled = var.config.shareable_link_enabled
  tunneling_enabled      = var.config.tunneling_enabled
  tags                   = var.config.tags

  ip_configuration {
    name                 = "configuration"
    subnet_id            = var.config.subnet_id
    public_ip_address_id = azurerm_public_ip.bastion.id
  }
}

resource "azurerm_monitor_diagnostic_setting" "bastion" {
  name                       = "${var.config.name}-diag"
  target_resource_id         = azurerm_bastion_host.bastion.id
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

resource "azurerm_monitor_diagnostic_setting" "bastion_public_ip" {
  name                       = "${var.config.name}-pip-diag"
  target_resource_id         = azurerm_public_ip.bastion.id
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