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

variable "sku_name" {
  description = "The SKU name of the Azure Firewall. Possible values are AZFW_Hub and AZFW_VNet"
  type        = string
  default     = "AZFW_VNet"
}

variable "sku_tier" {
  description = "The SKU tier of the Azure Firewall. Possible values are Standard and Premium"
  type        = string
  default     = "Standard"
}

variable "subnet_id" {
  description = "The ID of the subnet for Azure Firewall"
  type        = string
}

variable "enable_force_tunneling" {
  description = "Enable force tunneling for Azure Firewall"
  type        = bool
  default     = true
}

variable "force_tunneling_subnet_id" {
  description = "The ID of the subnet for Azure Firewall's force tunneling. This is always required."
  type        = string
}

variable "network_rules" {
  description = "List of network rules to apply to the firewall"
  type = list(object({
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
  }))
  default = []
}

variable "application_rules" {
  description = "List of application rules to apply to the firewall"
  type = list(object({
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
  }))
  default = []
}

variable "nat_rules" {
  description = "List of NAT rules to apply to the firewall"
  type = list(object({
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
  }))
  default = []
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