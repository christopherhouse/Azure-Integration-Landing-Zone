output "apim_private_ip" {
  value = module.apim.private_ip
}

output "azure_firewall_private_ip" {
  value = module.azure_firewall.private_ip
}