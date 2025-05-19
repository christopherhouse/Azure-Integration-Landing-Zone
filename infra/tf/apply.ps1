$resourceGroup = "RG-AIS-LZ-TF"
$storageAccount = "saaislztf" # <-- replace with your actual storage account name if different

az storage account update `
  --name $storageAccount `
  --resource-group $resourceGroup `
  --public-network-access Enabled

terraform apply ..\tf.plan

az storage account update `
  --name $storageAccount `
  --resource-group $resourceGroup `
  --public-network-access Disabled