variable "ase_name" {
  description = "Name of the App Service Environment (ASE)"
  type        = string
}

variable "resource_group_name" {
  description = "Resource group for ASE and DNS zone"
  type        = string
}

variable "location" {
  description = "Azure region for ASE"
  type        = string
}

variable "subnet_id" {
  description = "ID of the subnet to attach ASE to"
  type        = string
}
