variable "name" {
  description = "The name of the Event Hub namespace."
  type        = string
}

variable "location" {
  description = "The Azure region where the Event Hub namespace should be created."
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group in which to create the Event Hub namespace."
  type        = string
}

variable "config" {
  description = "Configuration for Event Hub resources"
  type = object({
    capacity = optional(number, 1)
    event_hubs = optional(list(object({
      name              = string
      partition_count   = optional(number, 2)
      message_retention = optional(number, 1)
      consumer_groups = optional(list(object({
        name          = string
        user_metadata = optional(string)
      })), [])
    })), [])
  })
  validation {
    condition     = contains([1, 2, 4, 8, 12, 20, 40], var.config.capacity)
    error_message = "Capacity must be one of: 1, 2, 4, 8, 12, 20, or 40."
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