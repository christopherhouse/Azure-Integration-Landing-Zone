variable "app_service_environment_name" {
  description = "Name of the App Service Environment (ASE)"
  type        = string
}

variable "resource_group_name" {
  description = "Resource group for App Service Environment and DNS zone"
  type        = string
}

variable "location" {
  description = "Azure region for App Service Environment"
  type        = string
}

variable "subnet_id" {
  description = "ID of the subnet to attach App Service Environment to"
  type        = string
}

variable "vnet_id" {
  description = "ID of the virtual network to link the DNS zone to"
  type        = string
}

variable "tags" {
  description = "A map of tags to assign to resources."
  type        = map(string)
}
