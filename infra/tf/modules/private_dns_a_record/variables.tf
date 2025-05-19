variable "name" {
  description = "The name of the A record."
  type        = string
}

variable "zone_name" {
  description = "The name of the private DNS zone."
  type        = string
}

variable "resource_group_name" {
  description = "The name of the resource group."
  type        = string
}

variable "ttl" {
  description = "The TTL for the A record."
  type        = number
  default     = 300
}

variable "records" {
  description = "The list of IP addresses for the A record."
  type        = list(string)
}

