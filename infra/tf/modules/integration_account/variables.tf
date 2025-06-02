variable "name" {
  description = "The name of the Logic App Integration Account."
  type        = string
}

variable "location" {
  description = "The Azure region where the Integration Account should be created."
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group in which to create the Integration Account."
  type        = string
}

variable "config" {
  description = "Configuration for Integration Account"
  type = object({
    sku_name = string
  })
  validation {
    condition     = contains(["Free", "Basic", "Standard", "Premium"], var.config.sku_name)
    error_message = "sku_name must be one of: Free, Basic, Standard, or Premium."
  }
}

variable "log_analytics_workspace_id" {
  description = "The ID of the Log Analytics workspace to which diagnostic logs will be sent."
  type        = string
}

variable "subnet_id" {
  description = "The ID of the subnet for the private endpoint (required for Premium SKU)."
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