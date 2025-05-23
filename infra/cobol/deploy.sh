#!/bin/bash

# Azure Integration Landing Zone COBOL Deployment Script
# This script demonstrates the complete deployment workflow

echo "Azure Integration Landing Zone - COBOL Deployment"
echo "=================================================="

# Set up environment
COBOL_DIR="/home/runner/work/Azure-Integration-Landing-Zone/Azure-Integration-Landing-Zone/infra/cobol"
CONFIG_FILE="$COBOL_DIR/config/azure-config.conf"

echo "Step 1: Validating environment..."
echo "----------------------------------"

# Check if configuration file exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ Configuration file not found: $CONFIG_FILE"
    echo "Please create and configure the file before deployment."
    exit 1
fi

echo "✓ Configuration file found"

# Check if COBOL programs are compiled
if [ ! -d "$COBOL_DIR/bin" ] || [ -z "$(ls -A $COBOL_DIR/bin 2>/dev/null)" ]; then
    echo "⚠️  Compiled programs not found. Building..."
    cd "$COBOL_DIR"
    ./build.sh
    
    if [ $? -ne 0 ]; then
        echo "❌ Build failed. Please resolve compilation issues."
        exit 1
    fi
fi

echo "✓ Programs are ready"

echo ""
echo "Step 2: Pre-deployment validation..."
echo "------------------------------------"

# Validate configuration file format
echo "Validating configuration file..."
if grep -q "SUBSCRIPTION_ID=" "$CONFIG_FILE" && \
   grep -q "RESOURCE_GROUP_NAME=" "$CONFIG_FILE" && \
   grep -q "LOCATION=" "$CONFIG_FILE"; then
    echo "✓ Configuration file format is valid"
else
    echo "❌ Configuration file is missing required settings"
    echo "Required settings: SUBSCRIPTION_ID, RESOURCE_GROUP_NAME, LOCATION"
    exit 1
fi

# Check for placeholder values
if grep -q "your-.*-here" "$CONFIG_FILE"; then
    echo "⚠️  Configuration contains placeholder values"
    echo "Please update the following placeholders in $CONFIG_FILE:"
    grep "your-.*-here" "$CONFIG_FILE"
    echo ""
    echo "Continuing with demo mode (ARM API calls will be simulated)..."
else
    echo "✓ Configuration appears to be properly set"
fi

echo ""
echo "Step 3: Running deployment..."
echo "-----------------------------"

cd "$COBOL_DIR"

# Run the main deployment program
if [ -f "bin/azuredeploy" ]; then
    echo "Executing COBOL deployment program..."
    ./bin/azuredeploy
    
    if [ $? -eq 0 ]; then
        echo ""
        echo "✅ Deployment completed successfully!"
    else
        echo ""
        echo "❌ Deployment failed. Check deployment.log for details."
        exit 1
    fi
else
    echo "❌ Deployment program not found. Please run ./build.sh first."
    exit 1
fi

echo ""
echo "Step 4: Post-deployment validation..."
echo "------------------------------------"

# Check if deployment log exists
if [ -f "deployment.log" ]; then
    echo "✓ Deployment log created"
    echo ""
    echo "Deployment Summary:"
    echo "==================="
    tail -10 deployment.log
else
    echo "⚠️  No deployment log found"
fi

echo ""
echo "🎉 COBOL Azure Integration Landing Zone Deployment Complete!"
echo ""
echo "Next steps:"
echo "1. Review deployment.log for detailed information"
echo "2. Verify resources in Azure portal"
echo "3. Test connectivity and functionality"
echo "4. Configure additional security settings as needed"

echo ""
echo "For troubleshooting, see: infra/cobol/README.md"