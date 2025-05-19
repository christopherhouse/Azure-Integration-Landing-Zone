variable "workspace_name" {
  description = "The name of the Log Analytics Workspace"
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group"
  type        = string
}

variable "location" {
  description = "The Azure region"
  type        = string
}

variable "tags" {
  description = "A map of tags to assign to resources."
  type        = map(string)
}
