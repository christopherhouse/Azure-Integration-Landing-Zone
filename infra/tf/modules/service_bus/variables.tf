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

variable "availability_zones" {
  description = "List of Availability Zones in which the Service Bus namespace will be deployed. Valid values are 1, 2, or 3 (or empty list for no AZ configuration)."
  type        = list(string)
  default     = []
  validation {
    condition = length(var.availability_zones) <= 3 && alltrue([
      for zone in var.availability_zones : contains(["1", "2", "3"], zone)
    ])
    error_message = "Availability zones must be a subset of [\"1\", \"2\", \"3\"] with at most 3 elements."
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