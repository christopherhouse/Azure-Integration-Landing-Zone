output "apim_private_ip" {
  value = var.deploy_api_management ? module.api_management[0].private_ip_addresses[0] : null
}

output "azure_firewall_private_ip" {
  value = var.azure_firewall.deploy_azure_firewall ? module.azure_firewall[0].firewall_private_ip : null
}

output "azure_firewall_public_ip" {
  value = var.azure_firewall.deploy_azure_firewall ? module.azure_firewall[0].public_ip_address : null
}