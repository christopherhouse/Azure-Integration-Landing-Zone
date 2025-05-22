module "naming" {
  source  = "Azure/naming/azurerm"
  version = "~> 0.4.0"
  suffix  = [var.suffix, var.environment]
  prefix  = var.workloadName != "" ? [var.workloadName] : []
}

