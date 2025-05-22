# GitHub Copilot Coding Agent Instructions

This document provides instructions for the GitHub Copilot Coding Agent when working with the Azure Integration Landing Zone repository. Follow these guidelines to ensure a consistent development workflow.

## Repository Overview

The Azure Integration Landing Zone repository contains Terraform configurations for deploying Azure resources required for an integration landing zone, including:

- Virtual Networks and Subnets
- API Management
- Service Bus
- Key Vault
- Storage Accounts
- Log Analytics
- Application Service Environment

## Development Workflow

Follow these steps when making changes to the Terraform configurations:

### 1. Understanding Repository Structure

```
/
├── .github/                 # GitHub-specific files and workflows
│   └── copilot-instructions.md  # This file
├── infra/                   # Infrastructure-related code
│   └── tf/                  # Terraform configurations
│       ├── modules/         # Reusable Terraform modules
│       ├── main.tf          # Main Terraform configuration
│       ├── variables.tf     # Variable definitions
│       ├── outputs.tf       # Output definitions
│       ├── terraform.tfvars # Variable values
│       ├── plan.ps1         # Script to run terraform plan
│       └── apply.ps1        # Script to run terraform apply
```

### 2. Making Changes to Terraform Files

1. Make changes to the appropriate Terraform files in the `infra/tf/` directory
2. Keep changes minimal and focused on specific requirements
3. Follow existing code style and patterns
4. Update documentation as needed

### 3. Testing Changes

Before committing any changes, you **MUST** perform the following steps:

1. Navigate to the Terraform directory:
   ```
   cd /home/runner/work/Azure-Integration-Landing-Zone/Azure-Integration-Landing-Zone/infra/tf
   ```

2. Initialize Terraform (if needed):
   ```
   terraform init
   ```

3. Run Terraform validate:
   ```
   terraform validate
   ```

4. Run Terraform plan using the Azure Storage Account for state:
   ```
   ./plan.ps1
   ```
   
   This script:
   - Temporarily enables public network access to the Storage Account
   - Runs `terraform plan -out=tf.plan`
   - Disables public network access to the Storage Account

5. Review the plan output carefully for:
   - Resources being created, modified, or destroyed
   - Any warnings or errors
   - Unintended changes

### 4. Handling Terraform Plan Failures

If `terraform plan` reports errors:

1. **DO NOT PROCEED** with committing the changes
2. Identify and fix the issues in your Terraform code
3. Re-run `terraform validate` and `terraform plan` until no errors are reported
4. Only then proceed with committing your changes

Common issues to watch for:
- Invalid resource configurations
- Missing required variables
- References to non-existent resources
- Syntax errors in Terraform files

### 5. Committing Changes

Only commit changes after successfully running `terraform plan` without errors:

1. Stage your changes
2. Write a clear, descriptive commit message
3. Include reference to any related issues
4. Push the changes to the appropriate branch

## Important Notes

- **State Management**: This repository uses an Azure Storage Account for Terraform state management, configured in `main.tf`:
  ```hcl
  backend "azurerm" {
    resource_group_name  = "RG-AIS-LZ-TF"
    storage_account_name = "saaislztf"
    container_name       = "tfstate"
    key                  = "terraform.tfstate"
    use_azuread_auth     = true
  }
  ```

- **Authentication**: The GitHub Copilot Coding Agent environment is configured with necessary Azure credentials through GitHub Actions workflow (see `.github/workflows/copilot-setup-steps.yml`).

- **Never Commit**: 
  - Terraform state files (`.tfstate`, `.tfstate.backup`)
  - Secret values or credentials
  - Large binary files
  - Temporary or generated files

## Resources

- [Azure Integration Landing Zone README](../README.md)
- [Terraform Azure Provider Documentation](https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs)
- [GitHub Copilot Documentation](https://docs.github.com/en/copilot)