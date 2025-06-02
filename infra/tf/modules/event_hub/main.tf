resource "azurerm_eventhub_namespace" "this" {
  name                = var.name
  location            = var.location
  resource_group_name = var.resource_group_name
  sku                 = "Standard" # Standard SKU for production workloads
  capacity            = var.config.capacity

  # Disable public network access for security
  public_network_access_enabled = false

  tags = var.tags
}

resource "azurerm_eventhub" "this" {
  for_each          = { for eh in var.config.event_hubs : eh.name => eh }
  name              = each.value.name
  namespace_id      = azurerm_eventhub_namespace.this.id
  partition_count   = lookup(each.value, "partition_count", 2)
  message_retention = lookup(each.value, "message_retention", 1)
}

resource "azurerm_eventhub_consumer_group" "this" {
  for_each = {
    for consumer_group in flatten([
      for event_hub in var.config.event_hubs : [
        for cg in lookup(event_hub, "consumer_groups", []) : {
          event_hub_name = event_hub.name
          cg_name        = cg.name
          cg_config      = cg
        }
      ]
    ]) : "${consumer_group.event_hub_name}-${consumer_group.cg_name}" => consumer_group
  }

  name                = each.value.cg_name
  namespace_name      = azurerm_eventhub_namespace.this.name
  eventhub_name       = each.value.event_hub_name
  resource_group_name = var.resource_group_name

  user_metadata = lookup(each.value.cg_config, "user_metadata", null)

  depends_on = [azurerm_eventhub.this]
}

resource "azurerm_monitor_diagnostic_setting" "eventhub_diag" {
  name                           = "eh-diag"
  target_resource_id             = azurerm_eventhub_namespace.this.id
  log_analytics_workspace_id     = var.log_analytics_workspace_id
  log_analytics_destination_type = "Dedicated"

  enabled_log {
    category_group = "AllLogs"
  }

  enabled_log {
    category_group = "Audit"
  }
}

module "private_dns_zone" {
  source              = "../private_dns_zone"
  zone_name           = "privatelink.servicebus.windows.net"
  resource_group_name = var.resource_group_name
  link_name           = "eh-dns-link"
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
  private_connection_resource_id = azurerm_eventhub_namespace.this.id
  subresource_names              = ["namespace"]
  private_dns_zone_ids           = [module.private_dns_zone.zone_id]
  tags                           = var.tags
}