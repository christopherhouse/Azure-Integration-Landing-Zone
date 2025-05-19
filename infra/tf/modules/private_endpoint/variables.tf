variable "name" { type = string }
variable "location" { type = string }
variable "resource_group_name" { type = string }
variable "subnet_id" { type = string }
variable "connection_name" { type = string }
variable "private_connection_resource_id" { type = string }
variable "subresource_names" { type = list(string) }
variable "tags" {
  description = "A map of tags to assign to resources."
  type        = map(string)
}
