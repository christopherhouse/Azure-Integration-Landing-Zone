terraform {
  required_providers {
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 4"
    }
  }
  required_version = ">= 1.0.0"
  backend "azurerm" {
    resource_group_name  = "RG-AIS-LZ-TF"
    storage_account_name = "saaislztf"
    container_name       = "tfstate"
    key                  = "terraform.tfstate"
    use_azuread_auth     = true
  }
}

provider "azurerm" {
  features {}
  subscription_id = var.subscription_id
}

provider "azurerm" {
  alias = "cli_auth"
  features {}
  subscription_id = var.subscription_id
}

data "azurerm_resource_group" "rg" {
  name     = var.resource_group_name
}

module "names" {
  source = "./modules/names"
  suffix = var.suffix
  environment = var.environment
}

module "log_analytics" {
  source              = "./modules/log_analytics"
  workspace_name      = module.names.log_analytics_workspace_name
  resource_group_name = data.azurerm_resource_group.rg.name
  location            = data.azurerm_resource_group.rg.location
}

module "vnet" {
  source              = "./modules/vnet"
  vnet_name           = module.names.vnet_name
  location            = data.azurerm_resource_group.rg.location
  resource_group_name = data.azurerm_resource_group.rg.name
  address_spaces      = var.vnet_address_spaces
  subnets             = var.vnet_subnets
}

data "azurerm_client_config" "current" {}

module "key_vault" {
  source                    = "./modules/key_vault"
  key_vault_name            = module.names.key_vault_name
  location                  = data.azurerm_resource_group.rg.location
  resource_group_name       = data.azurerm_resource_group.rg.name
  tenant_id                 = data.azurerm_client_config.current.tenant_id
  purge_protection_enabled  = var.key_vault_purge_protection_enabled
  soft_delete_retention_days = var.key_vault_soft_delete_retention_days
  log_analytics_workspace_id = module.log_analytics.workspace_id
}
