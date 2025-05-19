# Set these variables to match your environment
$resourceGroup = "RG-AIS-LZ-TF"
$storageAccount = "saaislztf"

# Enable public network access
az storage account update `
  --name $storageAccount `
  --resource-group $resourceGroup `
  --public-network-access Enabled

terraform plan -out=tf.plan

# Disable public network access
az storage account update `
  --name $storageAccount `
  --resource-group $resourceGroup `
  --public-network-access Disabled