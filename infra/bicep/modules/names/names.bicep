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

// Common resource name function
@description('Get a unique name for a resource type')
func getResourceName(resourceType string) string {
  var maxLength = 80 // default max length
  var allowedCharacters = 'alphanumeric' // default allowed characters

  // Set specific constraints based on resource type
  if (resourceType == 'storageAccount') {
    maxLength = 24
    allowedCharacters = 'alphanumeric'
  }
  else if (resourceType == 'keyVault') {
    maxLength = 24
    allowedCharacters = 'alphanumeric'
  }
  else if (resourceType == 'logAnalyticsWorkspace') {
    maxLength = 63
    allowedCharacters = 'alphanumeric-'
  }
  else if (resourceType == 'vnet') {
    maxLength = 64
    allowedCharacters = 'alphanumeric-_.'
  }
  else if (resourceType == 'apiManagement') {
    maxLength = 50
    allowedCharacters = 'alphanumeric-'
  }
  else if (resourceType == 'appServiceEnvironment') {
    maxLength = 40
    allowedCharacters = 'alphanumeric-'
  }
  else if (resourceType == 'serviceBusNamespace') {
    maxLength = 50
    allowedCharacters = 'alphanumeric-'
  }

  // Combine prefix and suffix
  var nameParts = concat(prefix, [resourceType], suffixArray)
  var name = join(nameParts, '-')

  // Ensure the name meets Azure requirements
  return substring(name, 0, min(length(name), maxLength))
}

// Get resource names
var storageAccountName = replace(getResourceName('st'), '-', '')
var keyVaultName = getResourceName('kv')
var logAnalyticsWorkspaceName = getResourceName('law')
var vnetName = getResourceName('vnet')
var apiManagementName = getResourceName('apim')
var appServiceEnvironmentName = getResourceName('ase')
var serviceBusNamespaceName = getResourceName('sb')

// Outputs
output logAnalyticsWorkspaceName string = logAnalyticsWorkspaceName
output vnetName string = vnetName
output keyVaultName string = keyVaultName
output apiManagementName string = apiManagementName
output appServiceEnvironmentName string = appServiceEnvironmentName
output storageAccountName string = storageAccountName
output serviceBusNamespaceName string = serviceBusNamespaceName