# GitHub Copilot Coding Agent Customization

This repository includes configuration for the GitHub Copilot Coding Agent environment, enabling it to execute Terraform commands and access Azure Storage using repository secrets.

## Implementation Details

The customization is implemented using a GitHub Actions workflow in `.github/workflows/copilot-setup.yml` which runs when the Copilot Coding Agent is initialized.

## Features

### Terraform Installation
The workflow installs the latest version of Terraform and verifies the installation, enabling the agent to execute commands like `terraform plan` and `terraform apply`.

### Azure Access Configuration
The workflow sets environment variables from repository secrets for:
- Azure Storage Account credentials
- Azure authentication credentials (client ID, secret, tenant ID, subscription ID)

### Additional Tools
Also installs Azure CLI for enhanced Azure resource management capabilities.

## Required Repository Secrets

Repository administrators need to set up the following secrets:
- `AZURE_STORAGE_ACCOUNT`
- `AZURE_STORAGE_KEY`
- `AZURE_STORAGE_CONNECTION_STRING`
- `AZURE_CLIENT_ID`
- `AZURE_CLIENT_SECRET`
- `AZURE_TENANT_ID`
- `AZURE_SUBSCRIPTION_ID`

## Usage

When the Copilot Coding Agent runs, it will automatically have access to the Terraform and Azure resources configured in the workflow.

For more information on customizing the Copilot Coding Agent, see the [GitHub documentation](https://docs.github.com/en/copilot/customizing-copilot/customizing-the-development-environment-for-copilot-coding-agent).