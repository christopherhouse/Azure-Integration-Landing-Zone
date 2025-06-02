variable "config" {
  description = "Configuration for Azure Firewall deployment and rules"
  type = object({
    name                       = string
    location                   = string
    resource_group_name        = string
    subnet_id                  = string
    force_tunneling_subnet_id  = string
    log_analytics_workspace_id = string
    apim_subnet_cidr           = optional(string, "10.10.3.0/24")
    sku_name                   = optional(string, "AZFW_VNet")
    sku_tier                   = optional(string, "Standard")
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
      name              = string
      description       = optional(string)
      priority          = number
      action            = string
      source_addresses  = optional(list(string))
      source_ip_groups  = optional(list(string))
      destination_fqdns = optional(list(string))
      fqdn_tags         = optional(list(string))
      protocols = optional(list(object({
        port = string
        type = string
      })))
    })), [])
    enable_apim_dnat = optional(bool, false)
    apim_private_ip  = optional(string, "")
    tags             = optional(map(string), {})
  })
}