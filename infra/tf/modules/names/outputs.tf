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

output "api_management_name" {
  value = module.naming.api_management.name_unique
}

output "app_service_environment_name" {
  value = module.naming.app_service_environment.name_unique
}

output "storage_account_name" {
  value = replace(module.naming.storage_account.name_unique, "-", "")
}

output "service_bus_namespace_name" {
  value = module.naming.servicebus_namespace.name_unique
}

output "firewall_name" {
  value = module.naming.firewall.name_unique
}
