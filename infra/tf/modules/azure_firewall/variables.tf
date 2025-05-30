variable "name" {
  description = "The name of the Azure Firewall"
  type        = string
}

variable "location" {
  description = "The Azure region where the Azure Firewall should be created"
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group"
  type        = string
}

variable "subnet_id" {
  description = "The ID of the subnet for Azure Firewall"
  type        = string
}

variable "force_tunneling_subnet_id" {
  description = "The ID of the subnet for Azure Firewall's force tunneling. This is always required."
  type        = string
}

variable "firewall_config" {
  description = "Configuration for Azure Firewall deployment and rules"
  type = object({
    sku_name               = optional(string, "AZFW_VNet")
    sku_tier               = optional(string, "Standard")
    enable_force_tunneling = optional(bool, true)
    network_rules = optional(list(object({
      name                  = string
      description           = optional(string)
      priority              = number
      action                = string
      source_addresses      = optional(list(string))
      destination_addresses = optional(list(string))
      destination_ports     = list(string)
      source_ip_groups      = optional(list(string))
      destination_ip_groups = optional(list(string))
      protocols             = list(string)
    })), [])
    application_rules = optional(list(object({
      name             = string
      description      = optional(string)
      priority         = number
      action           = string
      source_addresses = optional(list(string))
      source_ip_groups = optional(list(string))
      destination_fqdns = optional(list(string))
      fqdn_tags        = optional(list(string))
      protocols = optional(list(object({
        port = string
        type = string
      })))
    })), [])
    nat_rules = optional(list(object({
      name                = string
      description         = optional(string)
      priority            = number
      action              = string
      source_addresses    = optional(list(string))
      destination_address = string
      destination_ports   = list(string)
      source_ip_groups    = optional(list(string))
      protocols           = list(string)
      translated_address  = string
      translated_port     = string
    })), [])
  })
}

variable "log_analytics_workspace_id" {
  description = "The ID of the Log Analytics workspace to which diagnostic logs will be sent"
  type        = string
}

variable "tags" {
  description = "A map of tags to assign to resources"
  type        = map(string)
  default     = {}
}