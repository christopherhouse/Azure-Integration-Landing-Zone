output "namespace_id" {
  description = "The ID of the Event Hub namespace."
  value       = azurerm_eventhub_namespace.this.id
}

output "namespace_name" {
  description = "The name of the Event Hub namespace."
  value       = azurerm_eventhub_namespace.this.name
}

output "primary_connection_string" {
  description = "The primary connection string for the Event Hub namespace."
  value       = azurerm_eventhub_namespace.this.default_primary_connection_string
  sensitive   = true
}

output "event_hub_ids" {
  description = "Map of event hub names to their resource IDs"
  value       = { for name, eh in azurerm_eventhub.this : name => eh.id }
}

output "consumer_group_ids" {
  description = "Map of consumer group identifiers to their resource IDs"
  value       = { for id, cg in azurerm_eventhub_consumer_group.this : id => cg.id }
}