variable "zone_name" { type = string }
variable "resource_group_name" { type = string }
variable "link_name" { type = string }
variable "vnet_id" { type = string }
variable "tags" {
  description = "A map of tags to assign to resources."
  type        = map(string)
}
