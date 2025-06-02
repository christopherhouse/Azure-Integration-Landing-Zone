output "bastion_id" {
  value       = azurerm_bastion_host.bastion.id
  description = "The ID of the Azure Bastion"
}

output "bastion_name" {
  value       = azurerm_bastion_host.bastion.name
  description = "The name of the Azure Bastion"
}

output "bastion_fqdn" {
  value       = azurerm_bastion_host.bastion.dns_name
  description = "The FQDN of the Azure Bastion"
}

output "public_ip_address" {
  value       = azurerm_public_ip.bastion.ip_address
  description = "The public IP address of the Azure Bastion"
}