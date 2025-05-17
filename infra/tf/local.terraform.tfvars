resource_group_name = "RG-AIS-LZ-TF"
location            = "eastus2"
log_analytics_workspace_name = "log-ais-lz-tf"
subscription_id = "c5d4a6e8-69bf-4148-be25-cb362f83c370"
suffix       = "lz-tf"
environment  = "dev"

vnet_address_spaces = [
  "10.10.0.0/16"
]

vnet_subnets = [
  {
    name             = "ase"
    address_prefixes = ["10.10.1.0/24"]
    route_table      = null
    delegation = {
      name         = "ase-delegation"
      service_name = "Microsoft.Web/hostingEnvironments"
      actions      = ["Microsoft.Network/virtualNetworks/subnets/action"]
    }
    service_endpoints = []
  },
  {
    name             = "private-endpoints"
    address_prefixes = ["10.10.2.0/24"]
    nsg              = null
    route_table      = null
    delegation       = null
    service_endpoints = []
  },
  {
    name             = "apim"
    address_prefixes = ["10.10.3.0/24"]
    nsg              = null
    route_table      = null
    delegation       = null
    service_endpoints = []
  }
]

key_vault_purge_protection_enabled = true
key_vault_soft_delete_retention_days = 7