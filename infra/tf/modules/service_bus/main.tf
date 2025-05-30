resource "azurerm_servicebus_namespace" "this" {
  name                         = var.name
  location                     = var.location
  resource_group_name          = var.resource_group_name
  sku                          = "Premium" # Always Premium SKU as per requirement
  capacity                     = var.config.capacity_units
  premium_messaging_partitions = min(var.config.capacity_units, 4)

  tags = var.tags
}

resource "azurerm_servicebus_queue" "this" {
  for_each     = { for q in var.config.queues : q.name => q }
  name         = each.value.name
  namespace_id = azurerm_servicebus_namespace.this.id

  max_size_in_megabytes                = lookup(each.value, "max_size_in_megabytes", null)
  default_message_ttl                  = lookup(each.value, "default_message_ttl", null)
  max_delivery_count                   = lookup(each.value, "max_delivery_count", null)
  lock_duration                        = lookup(each.value, "lock_duration", null)
  requires_duplicate_detection         = lookup(each.value, "requires_duplicate_detection", null)
  requires_session                     = lookup(each.value, "requires_session", null)
  dead_lettering_on_message_expiration = lookup(each.value, "dead_lettering_on_message_expiration", null)
}

resource "azurerm_servicebus_topic" "this" {
  for_each     = { for t in var.config.topics : t.name => t }
  name         = each.value.name
  namespace_id = azurerm_servicebus_namespace.this.id

  max_size_in_megabytes        = lookup(each.value, "max_size_in_megabytes", null)
  default_message_ttl          = lookup(each.value, "default_message_ttl", null)
  requires_duplicate_detection = lookup(each.value, "requires_duplicate_detection", null)
  support_ordering             = lookup(each.value, "support_ordering", null)
}

resource "azurerm_servicebus_subscription" "this" {
  for_each = {
    for subscription in flatten([
      for topic in var.config.topics : [
        for sub in lookup(topic, "subscriptions", []) : {
          topic_name = topic.name
          sub_name   = sub.name
          sub_config = sub
        }
      ]
    ]) : "${subscription.topic_name}-${subscription.sub_name}" => subscription
  }

  name     = each.value.sub_name
  topic_id = azurerm_servicebus_topic.this[each.value.topic_name].id

  max_delivery_count                        = lookup(each.value.sub_config, "max_delivery_count", null)
  default_message_ttl                       = lookup(each.value.sub_config, "default_message_ttl", null)
  lock_duration                             = lookup(each.value.sub_config, "lock_duration", null)
  dead_lettering_on_message_expiration      = lookup(each.value.sub_config, "dead_lettering_on_message_expiration", null)
  dead_lettering_on_filter_evaluation_error = lookup(each.value.sub_config, "dead_lettering_on_filter_evaluation_error", null)
  requires_session                          = lookup(each.value.sub_config, "requires_session", null)
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
  private_dns_zone_ids           = [module.private_dns_zone.zone_id]
  tags                           = var.tags
}