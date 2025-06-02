output "id" {
  value = azurerm_api_management.this.id
}
output "name" {
  value = azurerm_api_management.this.name
}
output "private_ip_addresses" {
  value = azurerm_api_management.this.private_ip_addresses
  description = "The private IP addresses of the API Management service"
}
