output "names" {
  value = module.naming
}

output "log_analytics_workspace_name" {
  value = module.naming.log_analytics_workspace.name_unique
}

output "vnet_name" {
  value = module.naming.virtual_network.name_unique
}

output "key_vault_name" {
  value = module.naming.key_vault.name_unique
}
