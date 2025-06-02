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
  storage_use_azuread = true
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

module "spoke_vnet" {
  source              = "./modules/vnet"
  vnet_name           = "${module.names.vnet_name}-spoke"
  location            = data.azurerm_resource_group.rg.location
  resource_group_name = data.azurerm_resource_group.rg.name
  address_spaces      = var.spoke_vnet_address_spaces
  subnets             = var.spoke_vnet_subnets
  tags                = var.tags
}

module "hub_vnet" {
  count               = var.azure_firewall.deploy_azure_firewall ? 1 : 0
  source              = "./modules/vnet"
  vnet_name           = "${module.names.vnet_name}-hub"
  location            = data.azurerm_resource_group.rg.location
  resource_group_name = data.azurerm_resource_group.rg.name
  address_spaces      = var.hub_vnet_address_spaces
  subnets             = var.hub_vnet_subnets
  tags                = var.tags
}

module "vnet_peering" {
  count                = var.azure_firewall.deploy_azure_firewall ? 1 : 0
  source               = "./modules/vnet_peering"
  resource_group_name  = data.azurerm_resource_group.rg.name
  hub_vnet_name        = module.hub_vnet[0].vnet_name
  hub_vnet_id          = module.hub_vnet[0].vnet_id
  spoke_vnet_name      = module.spoke_vnet.vnet_name
  spoke_vnet_id        = module.spoke_vnet.vnet_id
  hub_to_spoke_name    = "hub-to-spoke"
  spoke_to_hub_name    = "spoke-to-hub"
}

# Route table for APIM subnet - routes traffic through Azure Firewall when in hub/spoke topology
resource "azurerm_route_table" "apim_route_table" {
  count               = var.azure_firewall.deploy_azure_firewall ? 1 : 0
  name                = "apim-route-table"
  location            = data.azurerm_resource_group.rg.location
  resource_group_name = data.azurerm_resource_group.rg.name
  tags                = var.tags

  route {
    name           = "ApiManagementServiceRoute"
    address_prefix = "ApiManagement"
    next_hop_type  = "Internet"
  }

  route {
    name                   = "DefaultRoute"
    address_prefix         = "0.0.0.0/0"
    next_hop_type          = "VirtualAppliance"
    next_hop_in_ip_address = module.azure_firewall[0].firewall_private_ip
  }
}

# Associate route table with APIM subnet
resource "azurerm_subnet_route_table_association" "apim_route_table_association" {
  count          = var.azure_firewall.deploy_azure_firewall ? 1 : 0
  subnet_id      = module.spoke_vnet.subnet_ids["apim"]
  route_table_id = azurerm_route_table.apim_route_table[0].id
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
  vnet_id                    = module.spoke_vnet.vnet_id
  subnet_id                  = module.spoke_vnet.subnet_ids["private-endpoints"]
  tags                       = var.tags
}

module "azure_firewall" {
  count  = var.azure_firewall.deploy_azure_firewall ? 1 : 0
  source = "./modules/azure_firewall"
  config = {
    name                       = module.names.firewall_name
    location                   = data.azurerm_resource_group.rg.location
    resource_group_name        = data.azurerm_resource_group.rg.name
    subnet_id                  = module.hub_vnet[0].subnet_ids["AzureFirewallSubnet"]
    force_tunneling_subnet_id  = module.hub_vnet[0].subnet_ids["AzureFirewallManagementSubnet"]
    log_analytics_workspace_id = module.log_analytics.workspace_id
    sku_name                   = var.azure_firewall.sku_name
    sku_tier                   = var.azure_firewall.sku_tier
    network_rules              = var.azure_firewall.network_rules
    application_rules          = var.azure_firewall.application_rules
    nat_rules                  = var.azure_firewall.nat_rules
    tags                       = var.tags
  }
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
  subnet_id                       = module.spoke_vnet.subnet_ids["apim"]
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
  vnet_id                      = module.spoke_vnet.vnet_id
  subnet_id                    = module.spoke_vnet.subnet_ids["ase"]
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
  vnet_id                    = module.spoke_vnet.vnet_id
  subnet_id                  = module.spoke_vnet.subnet_ids["private-endpoints"]
  private_endpoints          = lookup(each.value, "private_endpoints", [])
  create_private_dns_zone    = lookup(each.value, "create_private_dns_zone", false)
  blob_containers            = lookup(each.value, "blob_containers", [])
  tables                     = lookup(each.value, "tables", [])
  queues                     = lookup(each.value, "queues", [])
  file_shares                = lookup(each.value, "file_shares", [])
  tags                       = var.tags
}

module "service_bus" {
  count                      = var.service_bus.deploy ? 1 : 0
  source                     = "./modules/service_bus"
  name                       = module.names.service_bus_namespace_name
  location                   = data.azurerm_resource_group.rg.location
  resource_group_name        = data.azurerm_resource_group.rg.name
  log_analytics_workspace_id = module.log_analytics.workspace_id
  subnet_id                  = module.spoke_vnet.subnet_ids["private-endpoints"]
  vnet_id                    = module.spoke_vnet.vnet_id
  config                     = var.service_bus
  tags                       = var.tags
}

module "event_hub" {
  count                      = var.event_hub.deploy ? 1 : 0
  source                     = "./modules/event_hub"
  name                       = module.names.event_hub_namespace_name
  location                   = data.azurerm_resource_group.rg.location
  resource_group_name        = data.azurerm_resource_group.rg.name
  log_analytics_workspace_id = module.log_analytics.workspace_id
  subnet_id                  = module.spoke_vnet.subnet_ids["private-endpoints"]
  vnet_id                    = module.spoke_vnet.vnet_id
  config                     = var.event_hub
  tags                       = var.tags
}

module "data_factory" {
  count                          = var.deploy_azure_data_factory ? 1 : 0
  source                         = "./modules/data_factory"
  name                           = module.names.data_factory_name
  location                       = data.azurerm_resource_group.rg.location
  resource_group_name            = data.azurerm_resource_group.rg.name
  enable_managed_virtual_network = true
  public_network_enabled         = var.data_factory_public_network_enabled
  managed_private_endpoints      = var.data_factory_managed_private_endpoints
  git_configuration              = var.data_factory_git_configuration
  identity_type                  = var.data_factory_identity_type
  user_assigned_identity_ids     = var.data_factory_user_assigned_identity_ids
  log_analytics_workspace_id     = module.log_analytics.workspace_id
  subnet_id                      = module.spoke_vnet.subnet_ids["private-endpoints"]
  vnet_id                        = module.spoke_vnet.vnet_id
  tags                           = var.tags
}
