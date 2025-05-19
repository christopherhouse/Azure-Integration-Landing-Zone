variable "name" {
  type = string
}

variable "resource_group_name" {
  type = string
}

variable "location" {
  type = string
}

variable "log_analytics_workspace_id" {
  type = string
}

variable "subnet_id" {
  type = string
}

variable "sku_name" {
  type        = string
  description = "APIM SKU tier. Allowed: Developer, Premium."
  validation {
    condition     = contains(["Developer", "Premium"], var.sku_name)
    error_message = "sku_name must be either \"Developer\" or \"Premium\"."
  }
}

variable "sku_capacity" {
  type        = number
  description = "APIM SKU capacity (instance count)."
}

variable "enable_system_assigned_identity" {
  type = bool
}

variable "user_assigned_identity_ids" {
  type = list(string)
}

variable "publisher_name" {
  type        = string
  description = "The publisher name for API Management"
}

variable "publisher_email" {
  type        = string
  description = "The publisher email for API Management"
}

variable "tags" {
  description = "A map of tags to assign to resources."
  type        = map(string)
}
