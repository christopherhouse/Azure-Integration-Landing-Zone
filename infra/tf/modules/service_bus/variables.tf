variable "name" {
  description = "The name of the Service Bus namespace."
  type        = string
}

variable "location" {
  description = "The Azure region where the Service Bus namespace should be created."
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group in which to create the Service Bus namespace."
  type        = string
}

variable "capacity_units" {
  description = "Specifies the capacity units for the Service Bus namespace (Premium tier only). Valid values are 1, 2, 4, 8, or 16."
  type        = number
  validation {
    condition     = contains([1, 2, 4, 8, 16], var.capacity_units)
    error_message = "Capacity units must be one of: 1, 2, 4, 8, or 16."
  }
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