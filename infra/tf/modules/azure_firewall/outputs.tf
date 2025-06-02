output "firewall_id" {
  value       = azurerm_firewall.this.id
  description = "The ID of the Azure Firewall"
}

output "firewall_name" {
  value       = azurerm_firewall.this.name
  description = "The name of the Azure Firewall"
}

output "firewall_private_ip" {
  value       = azurerm_firewall.this.ip_configuration[0].private_ip_address
  description = "The private IP address of the Azure Firewall"
}

output "public_ip_address" {
  value       = azurerm_public_ip.this.ip_address
  description = "The public IP address of the Azure Firewall"
}

output "firewall_policy_id" {
  value       = azurerm_firewall_policy.this.id
  description = "The ID of the Firewall Policy"
}

output "apim_ip_group_id" {
  value       = azurerm_ip_group.apim_subnet.id
  description = "The ID of the IP Group containing the APIM subnet address space"
}