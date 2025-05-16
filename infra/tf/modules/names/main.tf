module "naming" {
  source  = "Azure/naming/azurerm"
  version = "~> 0.4.0"
  prefix  = [var.prefix]
  suffix  = [var.suffix, var.environment]
}

locals {
  log_analytics_workspace_name = module.naming.log_analytics_workspace.name_unique
}
