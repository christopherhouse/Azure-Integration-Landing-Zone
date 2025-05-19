resource "azurerm_storage_account" "this" {
  name                     = var.storage_account_name
  resource_group_name      = var.resource_group_name
  location                 = var.location
  account_tier             = split("_", var.sku_name)[0]
  account_replication_type = split("_", var.sku_name)[1]
  account_kind             = var.account_kind
  access_tier              = var.access_tier
  min_tls_version          = var.min_tls_version
  public_network_access_enabled = false
  tags                     = var.tags
}

resource "azurerm_storage_container" "this" {
  for_each              = { for c in var.blob_containers : c.name => c }
  name                  = each.value.name
  storage_account_id    = azurerm_storage_account.this.id
  container_access_type = lookup(each.value, "container_access_type", "private")
  metadata              = lookup(each.value, "metadata", null)
}

resource "azurerm_storage_table" "this" {
  for_each             = { for t in var.tables : t.name => t }
  name                 = each.value.name
  storage_account_name = azurerm_storage_account.this.name
}

resource "azurerm_storage_queue" "this" {
  for_each             = { for q in var.queues : q.name => q }
  name                 = each.value.name
  storage_account_name = azurerm_storage_account.this.name
  metadata             = lookup(each.value, "metadata", null)
}

resource "azurerm_storage_share" "this" {
  for_each             = { for f in var.file_shares : f.name => f }
  name                 = each.value.name
  storage_account_name = azurerm_storage_account.this.name
  quota                = lookup(each.value, "quota", null)
  metadata             = lookup(each.value, "metadata", null)
}

resource "azurerm_private_endpoint" "this" {
  for_each = toset(var.private_endpoints)
  name                = "pe-${azurerm_storage_account.this.name}-${each.key}"
  location            = var.location
  resource_group_name = var.resource_group_name
  subnet_id           = var.subnet_id

  private_service_connection {
    name                           = "psc-${azurerm_storage_account.this.name}-${each.key}"
    private_connection_resource_id  = azurerm_storage_account.this.id
    subresource_names              = [each.key]
    is_manual_connection           = false
  }

  dynamic "private_dns_zone_group" {
    for_each = var.create_private_dns_zone ? [1] : []
    content {
      name                 = "pdz-${each.key}"
      private_dns_zone_ids = [azurerm_private_dns_zone.this[each.key].id]
    }
  }
}

resource "azurerm_private_dns_zone" "this" {
  for_each = var.create_private_dns_zone ? { for s in var.private_endpoints : s => s } : {}
  name     = "privatelink.${each.key}.core.windows.net"
  resource_group_name = var.resource_group_name
}

resource "azurerm_private_dns_zone_virtual_network_link" "this" {
  for_each = var.create_private_dns_zone ? { for s in var.private_endpoints : s => s } : {}
  name                  = "pdzvnetlink-${each.key}"
  resource_group_name   = var.resource_group_name
  private_dns_zone_name = azurerm_private_dns_zone.this[each.key].name
  virtual_network_id    = var.vnet_id
}

resource "azurerm_monitor_diagnostic_setting" "storage" {
  name                       = "diag-blob"
  target_resource_id         = "${azurerm_storage_account.this.id}/blobServices/default"
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "StorageRead"
  }
  enabled_log {
    category = "StorageWrite"
  }
  enabled_log {
    category = "StorageDelete"
  }
  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

resource "azurerm_monitor_diagnostic_setting" "storage_table" {
  name                       = "diag-table"
  target_resource_id         = "${azurerm_storage_account.this.id}/tableServices/default"
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "StorageRead"
  }
  enabled_log {
    category = "StorageWrite"
  }
  enabled_log {
    category = "StorageDelete"
  }
  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

resource "azurerm_monitor_diagnostic_setting" "storage_queue" {
  name                       = "diag-queue"
  target_resource_id         = "${azurerm_storage_account.this.id}/queueServices/default"
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "StorageRead"
  }
  enabled_log {
    category = "StorageWrite"
  }
  enabled_log {
    category = "StorageDelete"
  }
  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

resource "azurerm_monitor_diagnostic_setting" "storage_file" {
  name                       = "diag-file"
  target_resource_id         = "${azurerm_storage_account.this.id}/fileServices/default"
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "StorageRead"
  }
  enabled_log {
    category = "StorageWrite"
  }
  enabled_log {
    category = "StorageDelete"
  }
  metric {
    category = "AllMetrics"
    enabled  = true
  }
}

output "storage_account_name" {
  value = azurerm_storage_account.this.name
}
