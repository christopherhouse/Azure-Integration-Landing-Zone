output "key_vault_id" {
  value = azurerm_key_vault.this.id
}

output "key_vault_name" {
  value = azurerm_key_vault.this.name
}

output "private_endpoint_ip" {
  value = module.private_endpoint.private_ip_address
}

output "private_dns_zone_name" {
  value = module.private_dns_zone.zone_name
}
