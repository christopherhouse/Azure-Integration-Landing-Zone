output "namespace_id" {
  description = "The ID of the Service Bus namespace."
  value       = azurerm_servicebus_namespace.this.id
}

output "namespace_name" {
  description = "The name of the Service Bus namespace."
  value       = azurerm_servicebus_namespace.this.name
}

output "primary_connection_string" {
  description = "The primary connection string for the Service Bus namespace."
  value       = azurerm_servicebus_namespace.this.default_primary_connection_string
  sensitive   = true
}

output "queue_ids" {
  description = "Map of queue names to their resource IDs"
  value       = { for name, queue in azurerm_servicebus_queue.this : name => queue.id }
}

output "topic_ids" {
  description = "Map of topic names to their resource IDs"
  value       = { for name, topic in azurerm_servicebus_topic.this : name => topic.id }
}

output "subscription_ids" {
  description = "Map of subscription identifiers to their resource IDs"
  value       = { for id, sub in azurerm_servicebus_subscription.this : id => sub.id }
}