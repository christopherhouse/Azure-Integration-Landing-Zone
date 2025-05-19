variable "resource_type" {
  description = "Resource type for naming."
  type        = string
  default     = null
}

variable "suffix" {
  description = "Suffix for resource names."
  type        = string
}

variable "environment" {
  description = "Environment for resource names."
  type        = string
}

variable "workloadName" {
  description = "Workload name for resource names."
  type        = string
}
