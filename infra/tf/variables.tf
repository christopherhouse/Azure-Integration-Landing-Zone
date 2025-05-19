variable "resource_group_name" {
  description = "The name of the resource group"
  type        = string
}

variable "location" {
  description = "The Azure region to deploy resources"
  type        = string
  default     = "eastus"
}

variable "subscription_id" {
  description = "The Azure Subscription ID to use for the provider."
  type        = string
}

variable "suffix" {
  description = "Suffix for resource naming."
  type        = string
}

variable "environment" {
  description = "Environment for resource naming."
  type        = string
}

variable "vnet_address_spaces" {
  description = "Address spaces for the virtual network."
  type        = list(string)
}

variable "vnet_subnets" {
  description = "Subnets configuration for the virtual network."
  type = list(object({
    name             = string
    address_prefixes = list(string)
    nsg = optional(object({
      name           = string
      security_rules = list(object({
        name                         = string
        priority                     = number
        direction                    = string
        access                       = string
        protocol                     = string
        source_port_range            = optional(string)
        source_port_ranges           = optional(list(string))
        destination_port_range       = optional(string)
        destination_port_ranges      = optional(list(string))
        source_address_prefix        = optional(string)
        source_address_prefixes      = optional(list(string))
        destination_address_prefix   = optional(string)
        destination_address_prefixes = optional(list(string))
        description                  = string
      }))
    }))
    route_table = optional(object({
      name   = string
      routes = list(object({
        name                   = string
        address_prefix         = string
        next_hop_type          = string
        next_hop_in_ip_address = optional(string)
      }))
    }))
    delegation = optional(object({
      name         = string
      service_name = string
      actions      = list(string)
    }))
    service_endpoints = optional(list(string))
  }))
}

variable "key_vault_purge_protection_enabled" {
  description = "Enable purge protection for Key Vault."
  type        = bool
  default     = true
}

variable "key_vault_soft_delete_retention_days" {
  description = "The number of days that items should be retained for soft delete in Key Vault."
  type        = number
  default     = 7
}

variable "apim_publisher_name" {
  type        = string
  description = "Publisher name for API Management"
}

variable "apim_publisher_email" {
  type        = string
  description = "Publisher email for API Management"
}

variable "apim_sku_name" {
  type        = string
  description = "APIM SKU tier. Allowed: Developer, Premium."
  validation {
    condition     = contains(["Developer", "Premium"], var.apim_sku_name)
    error_message = "apim_sku_name must be \"Developer\" or \"Premium\"."
  }
}

variable "apim_sku_capacity" {
  type        = number
  description = "APIM SKU capacity (instance count)."
}

variable "deploy_api_management" {
  description = "Controls whether the API Management module is deployed"
  type        = bool
  default     = true
}

variable "deploy_app_service_environment" {
  description = "Controls whether the App Service Environment module is deployed"
  type        = bool
  default     = false
}
