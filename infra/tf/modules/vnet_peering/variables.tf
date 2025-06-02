variable "resource_group_name" {
  description = "The name of the resource group containing both VNets"
  type        = string
}

variable "hub_vnet_name" {
  description = "The name of the hub virtual network"
  type        = string
}

variable "hub_vnet_id" {
  description = "The resource ID of the hub virtual network"
  type        = string
}

variable "spoke_vnet_name" {
  description = "The name of the spoke virtual network"
  type        = string
}

variable "spoke_vnet_id" {
  description = "The resource ID of the spoke virtual network"
  type        = string
}

variable "hub_to_spoke_name" {
  description = "The name for the hub-to-spoke peering"
  type        = string
  default     = "hub-to-spoke"
}

variable "spoke_to_hub_name" {
  description = "The name for the spoke-to-hub peering"
  type        = string
  default     = "spoke-to-hub"
}
