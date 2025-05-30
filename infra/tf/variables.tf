variable "resource_group_name" {
  description = "The name of the resource group"
  type        = string
}

variable "location" {
  description = "The Azure region to deploy resources"
  type        = string
  default     = "eastus"
}

variable "subscription_id" {
  description = "The Azure Subscription ID to use for the provider."
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

variable "spoke_vnet_address_spaces" {
  description = "Address spaces for the spoke virtual network."
  type        = list(string)
}

variable "hub_vnet_address_spaces" {
  description = "Address spaces for the hub virtual network (used when Azure Firewall is deployed)."
  type        = list(string)
  default     = []
}

variable "spoke_vnet_subnets" {
  description = "Subnets configuration for the spoke virtual network."
  type = list(object({
    name             = string
    address_prefixes = list(string)
    nsg = optional(object({
      name = string
      security_rules = list(object({
        name                         = string
        priority                     = number
        direction                    = string
        access                       = string
        protocol                     = string
        source_port_range            = optional(string)
        source_port_ranges           = optional(list(string))
        destination_port_range       = optional(string)
        destination_port_ranges      = optional(list(string))
        source_address_prefix        = optional(string)
        source_address_prefixes      = optional(list(string))
        destination_address_prefix   = optional(string)
        destination_address_prefixes = optional(list(string))
        description                  = string
      }))
    }))
    route_table = optional(object({
      name = string
      routes = list(object({
        name                   = string
        address_prefix         = string
        next_hop_type          = string
        next_hop_in_ip_address = optional(string)
      }))
    }))
    delegation = optional(object({
      name         = string
      service_name = string
      actions      = list(string)
    }))
    service_endpoints = optional(list(string))
  }))
}

variable "hub_vnet_subnets" {
  description = "Subnets configuration for the hub virtual network (used when Azure Firewall is deployed)."
  type = list(object({
    name             = string
    address_prefixes = list(string)
    nsg = optional(object({
      name = string
      security_rules = list(object({
        name                         = string
        priority                     = number
        direction                    = string
        access                       = string
        protocol                     = string
        source_port_range            = optional(string)
        source_port_ranges           = optional(list(string))
        destination_port_range       = optional(string)
        destination_port_ranges      = optional(list(string))
        source_address_prefix        = optional(string)
        source_address_prefixes      = optional(list(string))
        destination_address_prefix   = optional(string)
        destination_address_prefixes = optional(list(string))
        description                  = string
      }))
    }))
    route_table = optional(object({
      name = string
      routes = list(object({
        name                   = string
        address_prefix         = string
        next_hop_type          = string
        next_hop_in_ip_address = optional(string)
      }))
    }))
    delegation = optional(object({
      name         = string
      service_name = string
      actions      = list(string)
    }))
    service_endpoints = optional(list(string))
  }))
  default = []
}

variable "key_vault_purge_protection_enabled" {
  description = "Enable purge protection for Key Vault."
  type        = bool
  default     = true
}

variable "key_vault_soft_delete_retention_days" {
  description = "The number of days that items should be retained for soft delete in Key Vault."
  type        = number
  default     = 7
}

variable "apim_publisher_name" {
  type        = string
  description = "Publisher name for API Management"
}

variable "apim_publisher_email" {
  type        = string
  description = "Publisher email for API Management"
}

variable "apim_sku_name" {
  type        = string
  description = "APIM SKU tier. Allowed: Developer, Premium."
  validation {
    condition     = contains(["Developer", "Premium"], var.apim_sku_name)
    error_message = "apim_sku_name must be \"Developer\" or \"Premium\"."
  }
}

variable "apim_sku_capacity" {
  type        = number
  description = "APIM SKU capacity (instance count)."
}

variable "deploy_api_management" {
  description = "Controls whether the API Management module is deployed"
  type        = bool
  default     = true
}

variable "deploy_app_service_environment" {
  description = "Controls whether the App Service Environment module is deployed"
  type        = bool
  default     = false
}

variable "storage_accounts" {
  description = "List of storage accounts to deploy. Each item is an object with all configuration."
  type = list(object({
    name_prefix              = string
    location                 = optional(string)
    sku_name                 = optional(string)
    account_kind             = optional(string)
    access_tier              = optional(string)
    min_tls_version          = optional(string)
    allow_blob_public_access = optional(bool)
    private_endpoints        = optional(list(string))
    create_private_dns_zone  = optional(bool)
    blob_containers = optional(list(object({
      name                  = string
      public_access         = optional(string)
      metadata              = optional(map(string))
      container_access_type = optional(string)
    })))
    tables = optional(list(object({
      name = string
    })))
    queues = optional(list(object({
      name     = string
      metadata = optional(map(string))
    })))
    file_shares = optional(list(object({
      name     = string
      quota    = optional(number)
      metadata = optional(map(string))
    })))
  }))
  default = []
}

variable "service_bus" {
  description = "Configuration for Service Bus deployment and resources"
  type = object({
    deploy = bool
    capacity_units = optional(number, 1)
    queues = optional(list(object({
      name                                 = string
      max_size_in_megabytes                = optional(number)
      default_message_ttl                  = optional(string)
      enable_partitioning                  = optional(bool)
      enable_express                       = optional(bool)
      max_delivery_count                   = optional(number)
      lock_duration                        = optional(string)
      requires_duplicate_detection         = optional(bool)
      requires_session                     = optional(bool)
      dead_lettering_on_message_expiration = optional(bool)
    })), [])
    topics = optional(list(object({
      name                         = string
      max_size_in_megabytes        = optional(number)
      default_message_ttl          = optional(string)
      enable_partitioning          = optional(bool)
      enable_express               = optional(bool)
      requires_duplicate_detection = optional(bool)
      support_ordering             = optional(bool)
      subscriptions = optional(list(object({
        name                                      = string
        max_delivery_count                        = optional(number)
        default_message_ttl                       = optional(string)
        lock_duration                             = optional(string)
        dead_lettering_on_message_expiration      = optional(bool)
        dead_lettering_on_filter_evaluation_error = optional(bool)
        requires_session                          = optional(bool)
      })), [])
    })), [])
  })
  default = {
    deploy = false
  }
  validation {
    condition     = contains([1, 2, 4, 8, 16], var.service_bus.capacity_units)
    error_message = "Capacity units must be one of: 1, 2, 4, 8, or 16."
  }
}

variable "azure_firewall" {
  description = "Configuration for Azure Firewall deployment and rules"
  type = object({
    deploy_azure_firewall = bool
    sku_name              = optional(string, "AZFW_VNet")
    sku_tier              = optional(string, "Standard")
    network_rules = optional(list(object({
      name                  = string
      description           = optional(string)
      priority              = number
      action                = string
      source_addresses      = optional(list(string))
      destination_addresses = optional(list(string))
      destination_ports     = list(string)
      source_ip_groups      = optional(list(string))
      destination_ip_groups = optional(list(string))
      protocols             = list(string)
    })), [])
    application_rules = optional(list(object({
      name              = string
      description       = optional(string)
      priority          = number
      action            = string
      source_addresses  = optional(list(string))
      source_ip_groups  = optional(list(string))
      destination_fqdns = optional(list(string))
      protocols = optional(list(object({
        port = string
        type = string
      })))
    })), [])
    nat_rules = optional(list(object({
      name                = string
      description         = optional(string)
      priority            = number
      action              = string
      source_addresses    = optional(list(string))
      destination_address = string
      destination_ports   = list(string)
      source_ip_groups    = optional(list(string))
      protocols           = list(string)
      translated_address  = string
      translated_port     = string
    })), [])
  })
  default = {
    deploy_azure_firewall = false
  }
}

variable "deploy_azure_data_factory" {
  description = "Controlers whether Data Factory is deployed or not"
  type        = bool
  default     = false
}

variable "data_factory_managed_private_endpoints" {
  description = "List of managed private endpoints to create in the Data Factory managed virtual network"
  type = list(object({
    name               = string
    target_resource_id = string
    subresource_name   = string
  fqdns = optional(list(string), []) }))
}

variable "data_factory_git_configuration" {
  description = "Git configuration for the Data Factory"
  type = object({
    account_name    = string
    repository_name = string
    branch_name     = string
    root_folder     = string
  })
  default = null
}

variable "data_factory_public_network_enabled" {
  description = "Specifies whether public access is allowed to the Data Factory."
  type        = bool
  default     = false
}

variable "data_factory_identity_type" {
  description = "The type of identity to use for the Data Factory. Valid values are 'SystemAssigned' or 'UserAssigned' or 'SystemAssigned,UserAssigned'"
  type        = string
  default     = "SystemAssigned"
}

variable "data_factory_user_assigned_identity_ids" {
  description = "List of user-assigned managed identity IDs to associate with the Data Factory."
  type        = list(string)
  default     = []
}

variable "event_hub" {
  description = "Configuration for Event Hub deployment and resources"
  type = object({
    deploy   = bool
    capacity = optional(number, 1)
    event_hubs = optional(list(object({
      name              = string
      partition_count   = optional(number, 2)
      message_retention = optional(number, 1)
      consumer_groups = optional(list(object({
        name          = string
        user_metadata = optional(string)
      })), [])
    })), [])
  })
  default = {
    deploy = false
  }
  validation {
    condition     = contains([1, 2, 4, 8, 12, 20, 40], var.event_hub.capacity)
    error_message = "Capacity must be one of: 1, 2, 4, 8, 12, 20, or 40."
  }
}

variable "tags" {
  description = "A map of tags to assign to all resources."
  type        = map(string)
  default     = {}
}
