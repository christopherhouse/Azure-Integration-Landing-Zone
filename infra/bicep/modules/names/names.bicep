@description('Suffix for resource names')
param suffix string

@description('Environment for resource names')
param environment string

@description('Workload name for resource names')
param workloadName string = ''

var prefix = !empty(workloadName) ? [workloadName] : []
var suffixArray = [suffix, environment]

// Define resource name formats based on Azure naming best practices
// These functions mimic the Azure/naming/azurerm module used in Terraform

// Helper function to generate resource names
@description('Generate a resource name based on type and constraints')
var getResourceNameWithConstraints = {
  st: take(replace(join(concat(prefix, ['st'], suffixArray), '-'), '-', ''), 24)
  kv: take(join(concat(prefix, ['kv'], suffixArray), '-'), 24)
  law: take(join(concat(prefix, ['law'], suffixArray), '-'), 63)
  vnet: take(join(concat(prefix, ['vnet'], suffixArray), '-'), 64)
  apim: take(join(concat(prefix, ['apim'], suffixArray), '-'), 50)
  ase: take(join(concat(prefix, ['ase'], suffixArray), '-'), 40)
  sb: take(join(concat(prefix, ['sb'], suffixArray), '-'), 50)
}

// Get resource names
var storageAccountName = replace(getResourceNameWithConstraints.st, '-', '')
var keyVaultName = getResourceNameWithConstraints.kv
var logAnalyticsWorkspaceName = getResourceNameWithConstraints.law
var vnetName = getResourceNameWithConstraints.vnet
var apiManagementName = getResourceNameWithConstraints.apim
var appServiceEnvironmentName = getResourceNameWithConstraints.ase
var serviceBusNamespaceName = getResourceNameWithConstraints.sb

// Outputs
output logAnalyticsWorkspaceName string = logAnalyticsWorkspaceName
output vnetName string = vnetName
output keyVaultName string = keyVaultName
output apiManagementName string = apiManagementName
output appServiceEnvironmentName string = appServiceEnvironmentName
output storageAccountName string = storageAccountName
output serviceBusNamespaceName string = serviceBusNamespaceName