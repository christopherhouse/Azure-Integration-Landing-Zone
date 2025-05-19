output "id" {
  value = azurerm_storage_account.this.id
}

output "primary_blob_endpoint" {
  value = azurerm_storage_account.this.primary_blob_endpoint
}

output "private_endpoint_ids" {
  value = [for pe in azurerm_private_endpoint.this : pe.id]
}

output "private_dns_zone_ids" {
  value = var.create_private_dns_zone ? [for dz in azurerm_private_dns_zone.this : dz.id] : []
}
