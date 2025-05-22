output "data_factory_id" {
  description = "The ID of the Azure Data Factory."
  value       = azurerm_data_factory.this.id
}

output "data_factory_name" {
  description = "The name of the Azure Data Factory."
  value       = azurerm_data_factory.this.name
}

output "data_factory_identity" {
  description = "The identity of the Azure Data Factory, if configured."
  value       = try(azurerm_data_factory.this.identity[0], null)
}

output "private_endpoint_ip" {
  description = "The IP address of the private endpoint for the Data Factory."
  value       = module.private_endpoint.private_ip_address
}

output "private_dns_zone_name" {
  description = "The name of the private DNS zone for the Data Factory."
  value       = module.private_dns_zone.zone_name
}

output "managed_private_endpoint_ids" {
  description = "Map of managed private endpoint names to their IDs."
  value       = { for name, endpoint in azurerm_data_factory_managed_private_endpoint.this : name => endpoint.id }
}