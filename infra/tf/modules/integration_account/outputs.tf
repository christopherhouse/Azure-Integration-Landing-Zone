output "integration_account_id" {
  description = "The ID of the Logic App Integration Account"
  value       = azapi_resource.this.id
}

output "integration_account_name" {
  description = "The name of the Logic App Integration Account"
  value       = azapi_resource.this.name
}

output "integration_account_access_endpoint" {
  description = "The access endpoint of the Logic App Integration Account"
  value       = jsondecode(azapi_resource.this.output).properties.accessEndpoint
}

output "integration_account_principal_id" {
  description = "The principal ID of the system assigned managed identity"
  value       = jsondecode(azapi_resource.this.output).identity.principalId
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