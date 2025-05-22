# 🚀 Azure Integration Landing Zone - Pulumi Python Implementation

Welcome to the **Azure Integration Landing Zone Pulumi Python** implementation!  
This directory provides a modular, production-ready Pulumi codebase for deploying a secure, scalable Azure integration environment using Python.

---

## 📦 What's Inside?

- **Modular Pulumi Python Code**:  
  - Resource Group integration
  - Log Analytics Workspace
  - Virtual Network (with flexible subnet, NSG, route table, and delegation support)
  - Azure Key Vault (with RBAC, security best practices)
  - Azure API Management (internal VNet integration, identity, and logging)
  - Azure Service Bus (Premium tier, private endpoint, and availability zone support)
  - Consistent resource naming through a dedicated naming module

- **Best Practices**:  
  - Azure RBAC
  - Diagnostics capabilities
  - Soft delete & purge protection for Key Vault
  - Public access disabled where appropriate
  - Standard Python code style and structure

---

## 🛠️ How to Use

### 1. Prerequisites

- [Python](https://www.python.org/downloads/) 3.9 or later
- [Pulumi CLI](https://www.pulumi.com/docs/get-started/install/)
- [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli) installed and authenticated

### 2. Set Up Development Environment

```sh
# Clone the repo if you haven't already
git clone https://github.com/your-org/azure-integration-landing-zone.git
cd azure-integration-landing-zone/infra/pulumi/python

# Create and activate a virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### 3. Configure Pulumi

```sh
# Login to Pulumi
pulumi login

# Select or create a stack
pulumi stack select dev
# Or create a new stack: pulumi stack init dev
```

### 4. Configure Your Stack

Edit the `Pulumi.dev.yaml` file to set your configuration values:

```yaml
config:
  azure:location: eastus2
  azure-integration-landing-zone:resourceGroupName: RG-AIS-LZ
  azure-integration-landing-zone:suffix: lz
  azure-integration-landing-zone:environment: dev
  azure-integration-landing-zone:logAnalyticsWorkspaceName: log-ais-lz
  azure-integration-landing-zone:vnetAddressSpaces: ["10.10.0.0/16"]
  azure-integration-landing-zone:subscriptionId: "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
  # Optional configuration for API Management
  azure-integration-landing-zone:deployApiManagement: true
  azure-integration-landing-zone:apimPublisherName: "Your Company"
  azure-integration-landing-zone:apimPublisherEmail: "admin@example.com"
  azure-integration-landing-zone:apimSkuName: "Developer"
  azure-integration-landing-zone:apimSkuCapacity: 1
  # Optional configuration for Service Bus
  azure-integration-landing-zone:deployServiceBus: true
  azure-integration-landing-zone:serviceBusCapacityUnits: 1
```

### 5. Deploy the Infrastructure

```sh
# Preview changes
pulumi preview

# Deploy changes
pulumi up
```

### 6. Clean Up Resources (When No Longer Needed)

```sh
pulumi destroy
```

---

## 📚 Structure

```
infra/
  pulumi/
    python/
      lz/                        # Python package for landing zone
        __init__.py              # Package initialization
        __main__.py              # Main entry point
        names.py                 # Names module
        log_analytics.py         # Log Analytics module
        vnet.py                  # Virtual Network module
        key_vault.py             # Key Vault module
        api_management.py        # API Management module
        service_bus.py           # Service Bus module
        storage_account.py       # Storage Account module
      tests/                     # Unit tests
        __init__.py
        test_*.py                # Test modules
      Pulumi.yaml                # Pulumi project file
      Pulumi.dev.yaml            # Dev environment configuration
      requirements.txt           # Python dependencies
      pyproject.toml             # Python project configuration
      .pylintrc                  # Pylint configuration
      pytest.ini                 # Pytest configuration
```

---

## 🔹 API Management Module

> **This module is optional.**

The **API Management** module provisions an Azure API Management instance with:
- Internal VNet integration for secure, private access
- System-assigned and/or user-assigned managed identity support
- Flexible SKU and capacity configuration
- Best practices for security

This module is ideal for organizations needing centralized API gateway capabilities with enterprise-grade security and observability.

---

## 🔹 Service Bus Module

> **This module is optional.**

The **Service Bus** module deploys an Azure Service Bus namespace with:
- Premium tier for advanced features and throughput
- Private endpoint integration for secure connectivity
- Queue, topic, and subscription management
- High availability with multiple messaging entities
- Flexible configuration of messaging properties

This module provides enterprise messaging capabilities essential for asynchronous, decoupled communication patterns.

---

## 💡 Tips

- Use `pulumi config` command to set sensitive values like secrets:  
  `pulumi config set --secret azure-integration-landing-zone:apiKey YOUR_SECRET_VALUE`
- Run `black .` to format your Python code
- Run `pylint lz` to lint your code
- Run `pytest` to execute unit tests
- Use Azure CLI authentication for best experience:  
  `az login` before running Pulumi

---

## 📝 License

MIT

---

<p align="center">
  <img src="https://img.shields.io/badge/Azure-Integration-blue?logo=microsoftazure" />
  <img src="https://img.shields.io/badge/Pulumi-Ready-4C4CFF?logo=pulumi" />
  <img src="https://img.shields.io/badge/Python-3.9+-3776AB?logo=python" />
</p>