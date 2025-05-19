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
  type = string
}

variable "enable_system_assigned_identity" {
  type = bool
}

variable "user_assigned_identity_ids" {
  type = list(string)
}
