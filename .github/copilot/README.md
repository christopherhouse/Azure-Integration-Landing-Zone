# GitHub Copilot Coding Agent Customization

This directory contains files to customize the GitHub Copilot Coding Agent environment for the Azure Integration Landing Zone repository.

## Files

- `setup.sh`: Script to install and configure Terraform and Azure CLI in the agent environment
- `customization.json`: Configuration for environment variables and tool settings

## Required Repository Secrets

The following repository secrets need to be defined for the agent to access Azure resources:

- `AZURE_STORAGE_ACCOUNT`: The name of the Azure Storage Account
- `AZURE_STORAGE_KEY`: The access key for the Azure Storage Account
- `AZURE_STORAGE_CONNECTION_STRING`: The connection string for the Azure Storage Account
- `AZURE_CLIENT_ID`: The Azure AD client ID for authentication
- `AZURE_CLIENT_SECRET`: The client secret for Azure AD authentication
- `AZURE_TENANT_ID`: The Azure AD tenant ID
- `AZURE_SUBSCRIPTION_ID`: The Azure subscription ID

## Usage

When the Copilot Coding Agent runs, it will:

1. Execute the `setup.sh` script to install Terraform and Azure CLI
2. Set environment variables from the repository secrets as defined in `customization.json`
3. Enable the agent to run Terraform commands and access Azure resources

For more information on customizing the Copilot Coding Agent, see the [GitHub documentation](https://docs.github.com/en/copilot/customizing-copilot/customizing-the-development-environment-for-copilot-coding-agent).