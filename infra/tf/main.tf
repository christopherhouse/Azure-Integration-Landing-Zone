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

# Existing RG
data "azurerm_resource_group" "rg" {
  name = var.resource_group_name
}

module "names" {
  source       = "./modules/names"
  suffix       = var.suffix
  environment  = var.environment
  workloadName = ""
}

module "log_analytics" {
  source              = "./modules/log_analytics"
  workspace_name      = module.names.log_analytics_workspace_name
  resource_group_name = data.azurerm_resource_group.rg.name
  location            = data.azurerm_resource_group.rg.location
  tags                = var.tags
}

module "vnet" {
  source              = "./modules/vnet"
  vnet_name           = module.names.vnet_name
  location            = data.azurerm_resource_group.rg.location
  resource_group_name = data.azurerm_resource_group.rg.name
  address_spaces      = var.vnet_address_spaces
  subnets             = var.vnet_subnets
  tags                = var.tags
}

data "azurerm_client_config" "current" {}

module "key_vault" {
  source                     = "./modules/key_vault"
  key_vault_name             = module.names.key_vault_name
  location                   = data.azurerm_resource_group.rg.location
  resource_group_name        = data.azurerm_resource_group.rg.name
  tenant_id                  = data.azurerm_client_config.current.tenant_id
  purge_protection_enabled   = var.key_vault_purge_protection_enabled
  soft_delete_retention_days = var.key_vault_soft_delete_retention_days
  log_analytics_workspace_id = module.log_analytics.workspace_id
  vnet_id                    = module.vnet.vnet_id
  subnet_id                  = module.vnet.subnet_ids["private-endpoints"]
  tags                       = var.tags
}

module "api_management" {
  count                           = var.deploy_api_management ? 1 : 0
  source                          = "./modules/api_management"
  name                            = module.names.api_management_name
  location                        = data.azurerm_resource_group.rg.location
  resource_group_name             = data.azurerm_resource_group.rg.name
  publisher_name                  = var.apim_publisher_name
  publisher_email                 = var.apim_publisher_email
  sku_name                        = var.apim_sku_name
  sku_capacity                    = var.apim_sku_capacity
  subnet_id                       = module.vnet.subnet_ids["apim"]
  log_analytics_workspace_id      = module.log_analytics.workspace_id
  enable_system_assigned_identity = true # or false, depending on your needs
  user_assigned_identity_ids      = []   # or provide actual IDs if needed
  tags                            = var.tags
}

module "app_service_environment" {
  count                        = var.deploy_app_service_environment ? 1 : 0
  source                       = "./modules/app_service_environment"
  app_service_environment_name = module.names.app_service_environment_name
  resource_group_name          = data.azurerm_resource_group.rg.name
  location                     = var.location
  vnet_id                      = module.vnet.vnet_id
  subnet_id                    = module.vnet.subnet_ids["ase"]
  tags                         = var.tags
}

# Storage Accounts
resource "null_resource" "storage_account_names" {
  for_each = { for sa in var.storage_accounts : sa.name_prefix => sa }
}

module "storage_account_names" {
  source       = "./modules/names"
  for_each     = { for sa in var.storage_accounts : sa.name_prefix => sa }
  suffix       = var.suffix
  environment  = var.environment
  workloadName = each.value.name_prefix
}

module "storage_accounts" {
  source                     = "./modules/storage_account"
  for_each                   = { for sa in var.storage_accounts : sa.name_prefix => sa }
  log_analytics_workspace_id = module.log_analytics.workspace_id
  storage_account_name       = module.storage_account_names[each.key].storage_account_name
  location                   = data.azurerm_resource_group.rg.location
  resource_group_name        = var.resource_group_name
  sku_name                   = lookup(each.value, "sku_name", "Standard_LRS")
  account_kind               = lookup(each.value, "account_kind", "StorageV2")
  access_tier                = lookup(each.value, "access_tier", "Hot")
  min_tls_version            = lookup(each.value, "min_tls_version", "TLS1_2")
  allow_blob_public_access   = lookup(each.value, "allow_blob_public_access", false)
  vnet_id                    = module.vnet.vnet_id
  subnet_id                  = module.vnet.subnet_ids["private-endpoints"]
  private_endpoints          = lookup(each.value, "private_endpoints", [])
  create_private_dns_zone    = lookup(each.value, "create_private_dns_zone", false)
  blob_containers            = lookup(each.value, "blob_containers", [])
  tables                     = lookup(each.value, "tables", [])
  queues                     = lookup(each.value, "queues", [])
  file_shares                = lookup(each.value, "file_shares", [])
  tags                       = var.tags
}

module "service_bus" {
  count                      = var.deploy_service_bus ? 1 : 0
  source                     = "./modules/service_bus"
  name                       = module.names.service_bus_namespace_name
  location                   = data.azurerm_resource_group.rg.location
  resource_group_name        = data.azurerm_resource_group.rg.name
  capacity_units             = var.service_bus_capacity_units
  log_analytics_workspace_id = module.log_analytics.workspace_id
  subnet_id                  = module.vnet.subnet_ids["private-endpoints"]
  vnet_id                    = module.vnet.vnet_id
  queues                     = var.service_bus_queues
  topics                     = var.service_bus_topics
  tags                       = var.tags
}
