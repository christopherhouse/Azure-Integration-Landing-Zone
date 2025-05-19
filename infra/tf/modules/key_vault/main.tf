resource "azurerm_key_vault" "this" {
  name                          = var.key_vault_name
  location                      = var.location
  resource_group_name           = var.resource_group_name
  tenant_id                     = var.tenant_id
  enable_rbac_authorization     = true
  soft_delete_retention_days    = var.soft_delete_retention_days
  purge_protection_enabled      = var.purge_protection_enabled
  public_network_access_enabled = false
  sku_name                      = "standard"
}

resource "azurerm_monitor_diagnostic_setting" "kv_diag" {
  name                       = "kv-diag"
  target_resource_id         = azurerm_key_vault.this.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "AuditEvent"
  }

  metric {
    category = "AllMetrics"
  }
}

module "private_dns_zone" {
  source              = "../private_dns_zone"
  zone_name           = "privatelink.vaultcore.azure.net"
  resource_group_name = var.resource_group_name
  link_name           = "kv-dns-link"
  vnet_id             = var.vnet_id
}

module "private_endpoint" {
  source                        = "../private_endpoint"
  name                          = "${var.key_vault_name}-pe"
  location                      = var.location
  resource_group_name           = var.resource_group_name
  subnet_id                     = var.subnet_id
  connection_name               = "${var.key_vault_name}-pe-conn"
  private_connection_resource_id = azurerm_key_vault.this.id
  subresource_names             = ["vault"]
}
