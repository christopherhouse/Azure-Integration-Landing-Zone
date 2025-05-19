output "ase_id" {
  value = azurerm_app_service_environment_v3.ase.id
}

output "ase_name" {
  value = azurerm_app_service_environment_v3.ase.name
}

output "ase_ip" {
  value = azurerm_app_service_environment_v3.ase.internal_inbound_ip_address
}
