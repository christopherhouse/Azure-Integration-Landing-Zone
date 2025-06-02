variable "config" {
  description = "Configuration for Azure Bastion deployment"
  type = object({
    name                       = string
    location                   = string
    resource_group_name        = string
    subnet_id                  = string
    log_analytics_workspace_id = string
    sku                        = optional(string, "Standard")
    copy_paste_enabled         = optional(bool, true)
    file_copy_enabled          = optional(bool, true)
    ip_connect_enabled         = optional(bool, true)
    shareable_link_enabled     = optional(bool, false)
    tunneling_enabled          = optional(bool, true)
    tags                       = optional(map(string), {})
  })
}