variable "resource_group_name" {
  description = "The name of the resource group"
  type        = string
}

variable "location" {
  description = "The Azure region to deploy resources"
  type        = string
  default     = "eastus"
}

variable "log_analytics_workspace_name" {
  description = "The name of the Log Analytics Workspace"
  type        = string
}

variable "subscription_id" {
  description = "The Azure Subscription ID to use for the provider."
  type        = string
}

variable "prefix" {
  description = "Prefix for resource naming."
  type        = string
}

variable "suffix" {
  description = "Suffix for resource naming."
  type        = string
}

variable "environment" {
  description = "Environment for resource naming."
  type        = string
}
