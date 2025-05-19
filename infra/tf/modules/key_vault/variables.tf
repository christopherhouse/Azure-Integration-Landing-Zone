variable "key_vault_name" {
  description = "The name of the Key Vault."
  type        = string
}

variable "location" {
  description = "Azure region."
  type        = string
}

variable "resource_group_name" {
  description = "Resource group name."
  type        = string
}

variable "tenant_id" {
  description = "Azure AD tenant ID."
  type        = string
}

variable "enable_soft_delete" {
  description = "Enable soft delete for Key Vault."
  type        = bool
  default     = true
}

variable "purge_protection_enabled" {
  description = "Enable purge protection for Key Vault."
  type        = bool
  default     = true
}

variable "log_analytics_workspace_id" {
  description = "Log Analytics Workspace ID for diagnostics."
  type        = string
}

variable "soft_delete_retention_days" {
  description = "The number of days that items should be retained for soft delete."
  type        = number
  default     = 7
}

variable "vnet_id" {
  description = "The ID of the Virtual Network to link the DNS zone and for the private endpoint."
  type        = string
}

variable "subnet_id" {
  description = "The subnet ID for the Key Vault private endpoint."
  type        = string
}
