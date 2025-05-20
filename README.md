# 🚀 Azure Integration Landing Zone

Welcome to the **Azure Integration Landing Zone**!  
This repository provides a modular, production-ready Terraform codebase for deploying a secure, scalable Azure integration environment.

---

## 📦 What’s Inside?

- **Modular Terraform Code**:  
  - Resource Group
  - Log Analytics Workspace
  - Virtual Network (with flexible subnet, NSG, route table, and delegation support)
  - Azure Key Vault (with RBAC, diagnostics, and security best practices)
  - Azure API Management (internal VNet integration, diagnostics, identity, and logging)
  - Azure Service Bus (Premium tier, private endpoint, and availability zone support)
  - Azure Naming module integration for consistent resource names

- **Best Practices**:  
  - Azure RBAC
  - Diagnostics routed to Log Analytics
  - Soft delete & purge protection for Key Vault
  - Public access disabled where appropriate

---

## 🛠️ How to Use

### 1. Clone the Repo

```sh
git clone https://github.com/your-org/azure-integration-landing-zone.git
cd azure-integration-landing-zone/infra/tf
```

### 2. Configure Your `terraform.tfvars`

Example:

```hcl
resource_group_name = "RG-AIS-LZ-TF"
location            = "eastus2"
log_analytics_workspace_name = "log-ais-lz-tf"
subscription_id = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
suffix       = "lz"
environment  = "dev"

vnet_address_spaces = [
  "10.10.0.0/16"
]

vnet_subnets = [
  {
    name             = "ase"
    address_prefixes = ["10.10.1.0/24"]
    route_table      = null
    delegation = {
      name         = "ase-delegation"
      service_name = "Microsoft.Web/hostingEnvironments"
      actions      = ["Microsoft.Network/virtualNetworks/subnets/action"]
    }
    service_endpoints = []
  },
  {
    name             = "private-endpoints"
    address_prefixes = ["10.10.2.0/24"]
    nsg              = null
    route_table      = null
    delegation       = null
    service_endpoints = []
  }
]

key_vault_purge_protection_enabled   = true
key_vault_soft_delete_retention_days = 7

# ---
# API Management (APIM) module configuration (optional)
# Remove or comment out this section if you do not wish to deploy APIM
# ---
apim_enabled = true
apim_name = "apimlz-tfdevoluo"
apim_sku_name = "Developer"
apim_sku_capacity = 1
apim_publisher_name = "Your Company"
apim_publisher_email = "admin@yourcompany.com"
apim_subnet_id = "/subscriptions/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/resourceGroups/RG-AIS-LZ-TF/providers/Microsoft.Network/virtualNetworks/vnet-ais-lz-tf/subnets/apim"
apim_enable_system_assigned_identity = true
apim_user_assigned_identity_ids = []

# ---
# Service Bus module configuration (optional)
# Remove or comment out this section if you do not wish to deploy Service Bus
# ---
deploy_service_bus = true
service_bus_capacity_units = 1
service_bus_availability_zones = ["1", "2", "3"]
```

> **Note:** The API Management (APIM) module is optional. If you do not wish to deploy APIM, set `deploy_api_management = false` or remove the APIM-related variables from your `terraform.tfvars`.

> **Note:** The Service Bus module is optional. If you do not wish to deploy Service Bus, set `deploy_service_bus = false` or remove the Service Bus-related variables from your `terraform.tfvars`.

### 3. Initialize & Deploy

```sh
terraform init
terraform plan -out tfplan
terraform apply tfplan
```

---

## 📚 Structure

```
infra/
  tf/
    main.tf
    variables.tf
    terraform.tfvars
    modules/
      names/
      log_analytics/
      vnet/
      key_vault/
      api_management/   # Azure API Management module (optional)
      service_bus/      # Azure Service Bus module (optional)
```

---

## 🔹 API Management Module

> **This module is optional.**

The **API Management** module provisions an Azure API Management instance with:
- Internal VNet integration for secure, private access
- System-assigned and/or user-assigned managed identity support
- Diagnostic settings routed to Log Analytics (audit, gateway, websocket logs, metrics)
- Flexible SKU and capacity configuration
- Best practices for security and monitoring

This module is ideal for organizations needing centralized API gateway capabilities with enterprise-grade security and observability.

---

## 🔹 Service Bus Module

> **This module is optional.**

The **Service Bus** module provisions an Azure Service Bus namespace with:
- Premium SKU for enterprise-grade messaging capabilities
- Private endpoint integration for secure, private access
- Configurable capacity units (1, 2, 4, 8, 16)
- Optional Availability Zone redundancy (support for 1-3 AZs)
- Diagnostic settings routed to Log Analytics
- Private DNS zone integration

This module is ideal for organizations requiring a robust messaging infrastructure with enterprise-grade security, reliability, and scalability.

---

## 💡 Tips

- All resource names are generated using the [Azure Naming Terraform Module](https://registry.terraform.io/modules/Azure/naming/azurerm/latest).
- Sensitive files like `*.tfvars` and state files are gitignored.
- Use Azure CLI authentication for best experience:  
  `az login` before running Terraform.

---

## 📝 License

MIT

---

<p align="center">
  <img src="https://img.shields.io/badge/Azure-Integration-blue?logo=microsoftazure" />
  <img src="https://img.shields.io/badge/Terraform-Ready-623CE4?logo=terraform" />
</p>
