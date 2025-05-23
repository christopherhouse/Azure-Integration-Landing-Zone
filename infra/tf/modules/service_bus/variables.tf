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

variable "queues" {
  description = "List of Service Bus queues to create"
  type = list(object({
    name                                 = string
    max_size_in_megabytes                = optional(number)
    default_message_ttl                  = optional(string)
    max_delivery_count                   = optional(number)
    lock_duration                        = optional(string)
    requires_duplicate_detection         = optional(bool)
    requires_session                     = optional(bool)
    dead_lettering_on_message_expiration = optional(bool)
  }))
  default = [
    {
      name                  = "orders-queue"
      max_size_in_megabytes = 1024
      default_message_ttl   = "P14D" # 14 days
      max_delivery_count    = 10
    },
    {
      name                                 = "notifications-queue"
      max_size_in_megabytes                = 1024
      default_message_ttl                  = "P7D" # 7 days
      max_delivery_count                   = 5
      requires_session                     = true
      dead_lettering_on_message_expiration = true
    }
  ]
}

variable "topics" {
  description = "List of Service Bus topics to create"
  type = list(object({
    name                         = string
    max_size_in_megabytes        = optional(number)
    default_message_ttl          = optional(string)
    requires_duplicate_detection = optional(bool)
    support_ordering             = optional(bool)
    subscriptions = optional(list(object({
      name                                      = string
      max_delivery_count                        = optional(number)
      default_message_ttl                       = optional(string)
      lock_duration                             = optional(string)
      dead_lettering_on_message_expiration      = optional(bool)
      dead_lettering_on_filter_evaluation_error = optional(bool)
      requires_session                          = optional(bool)
    })), [])
  }))
  default = [
    {
      name                  = "events"
      max_size_in_megabytes = 1024
      default_message_ttl   = "P14D" # 14 days
      subscriptions = [
        {
          name               = "all-events"
          max_delivery_count = 10
        },
        {
          name                = "critical-events"
          max_delivery_count  = 20
          default_message_ttl = "P7D" # 7 days
          requires_session    = true
        }
      ]
    },
    {
      name                  = "alerts"
      max_size_in_megabytes = 1024
      subscriptions = [
        {
          name               = "system-alerts"
          max_delivery_count = 10
        },
        {
          name                                      = "security-alerts"
          max_delivery_count                        = 10
          dead_lettering_on_message_expiration      = true
          dead_lettering_on_filter_evaluation_error = true
        }
      ]
    }
  ]
}

variable "tags" {
  description = "A mapping of tags to assign to the resources."
  type        = map(string)
  default     = {}
}