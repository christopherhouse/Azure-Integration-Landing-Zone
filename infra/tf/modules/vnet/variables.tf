variable "vnet_name" {
  description = "Name of the virtual network."
  type        = string
}

variable "location" {
  description = "Azure region."
  type        = string
}

variable "resource_group_name" {
  description = "Resource group name."
  type        = string
}

variable "address_spaces" {
  description = "Array of address spaces for the VNet."
  type        = list(string)
}

variable "subnets" {
  description = "Array of subnet objects."
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

variable "tags" {
  description = "A map of tags to assign to resources."
  type        = map(string)
}
