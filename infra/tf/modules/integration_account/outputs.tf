output "integration_account_id" {
  description = "The ID of the Logic App Integration Account"
  value       = azurerm_logic_app_integration_account.this.id
}

output "integration_account_name" {
  description = "The name of the Logic App Integration Account"
  value       = azurerm_logic_app_integration_account.this.name
}

output "integration_account_access_endpoint" {
  description = "The access endpoint of the Logic App Integration Account"
  value       = azurerm_logic_app_integration_account.this.access_endpoint
}

output "integration_account_principal_id" {
  description = "The principal ID of the system assigned managed identity"
  value       = azurerm_logic_app_integration_account.this.identity[0].principal_id
}

output "storage_account_id" {
  description = "The ID of the storage account (Premium SKU only)"
  value       = var.config.sku_name == "Premium" ? azurerm_storage_account.integration_content[0].id : null
}

output "storage_account_name" {
  description = "The name of the storage account (Premium SKU only)"
  value       = var.config.sku_name == "Premium" ? azurerm_storage_account.integration_content[0].name : null
}

output "storage_account_primary_endpoint" {
  description = "The primary blob endpoint of the storage account (Premium SKU only)"
  value       = var.config.sku_name == "Premium" ? azurerm_storage_account.integration_content[0].primary_blob_endpoint : null
}