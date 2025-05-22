variable "name" {
  description = "The name of the Azure Data Factory."
  type        = string
}

variable "location" {
  description = "The Azure region where the Data Factory should be created."
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group in which to create the Data Factory."
  type        = string
}

variable "enable_managed_virtual_network" {
  description = "Specifies whether to enable managed virtual network for the Data Factory."
  type        = bool
  default     = true
}

variable "public_network_enabled" {
  description = "Specifies whether public access is allowed to the Data Factory."
  type        = bool
  default     = false
}

variable "managed_private_endpoints" {
  description = "List of managed private endpoints to create in the Data Factory managed virtual network"
  type = list(object({
    name               = string
    target_resource_id = string
    subresource_name   = string
    fqdns              = optional(list(string), [])
  }))
  default = []
}

variable "git_configuration" {
  description = "Git configuration for the Data Factory"
  type = object({
    account_name    = string
    repository_name = string
    branch_name     = string
    root_folder     = string
  })
  default = null
}

variable "identity_type" {
  description = "The type of identity to use for the Data Factory. Valid values are 'SystemAssigned' or 'UserAssigned' or 'SystemAssigned,UserAssigned'"
  type        = string
  default     = "SystemAssigned"
  validation {
    condition     = contains(["SystemAssigned", "UserAssigned", "SystemAssigned,UserAssigned"], var.identity_type)
    error_message = "The identity_type must be one of 'SystemAssigned', 'UserAssigned', or 'SystemAssigned,UserAssigned'."
  }
}

variable "user_assigned_identity_ids" {
  description = "List of user-assigned managed identity IDs to associate with the Data Factory."
  type        = list(string)
  default     = []
}

variable "log_analytics_workspace_id" {
  description = "The ID of the Log Analytics workspace to which diagnostic logs will be sent."
  type        = string
}

variable "subnet_id" {
  description = "The ID of the subnet for the private endpoint."
  type        = string
}

variable "vnet_id" {
  description = "The ID of the virtual network for the private DNS zone link."
  type        = string
}

variable "tags" {
  description = "A mapping of tags to assign to the resources."
  type        = map(string)
  default     = {}
}