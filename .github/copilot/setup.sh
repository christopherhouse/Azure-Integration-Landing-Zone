#!/bin/bash
# Custom setup script for GitHub Copilot Coding Agent
# This script installs Terraform and sets up Azure CLI for Azure Storage Account access

echo "Starting Copilot Coding Agent customization..."

# Install Terraform
echo "Installing Terraform..."
wget -O- https://apt.releases.hashicorp.com/gpg | gpg --dearmor | tee /usr/share/keyrings/hashicorp-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | tee /etc/apt/sources.list.d/hashicorp.list
apt-get update && apt-get install -y terraform

# Verify Terraform installation
terraform --version

# Install Azure CLI if not already installed
echo "Installing Azure CLI..."
if ! command -v az &> /dev/null; then
  curl -sL https://aka.ms/InstallAzureCLIDeb | bash
fi

# Verify Azure CLI installation
az --version

echo "Development environment customization completed."