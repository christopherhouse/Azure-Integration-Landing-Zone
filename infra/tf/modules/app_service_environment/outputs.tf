output "app_service_environment_id" {
  value = azurerm_app_service_environment_v3.app_service_environment.id
}

output "app_service_environment_name" {
  value = azurerm_app_service_environment_v3.app_service_environment.name
}

output "app_service_environment_ip" {
  value = azurerm_app_service_environment_v3.app_service_environment.internal_inbound_ip_addresses[0]
}
