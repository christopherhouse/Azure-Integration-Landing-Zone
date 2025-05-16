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

module "log_analytics" {
  source              = "./modules/log_analytics"
  workspace_name      = var.log_analytics_workspace_name
  resource_group_name = data.azurerm_resource_group.rg.name
  location            = data.azurerm_resource_group.rg.location
}
