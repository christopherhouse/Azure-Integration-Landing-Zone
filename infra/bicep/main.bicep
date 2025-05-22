// Deployment parameters
@description('The name of the existing resource group')
param resourceGroupName string

@description('The location to deploy resources')
param location string = resourceGroup().location

@description('Suffix for resource naming')
param suffix string

@description('Environment for resource naming')
param environment string

@description('VNet address spaces')
param vnetAddressSpaces array

@description('Subnet configuration')
param vnetSubnets array

@description('Tags for resources')
param tags object = {}

// Key Vault parameters
@description('Enable purge protection for Key Vault')
param keyVaultPurgeProtectionEnabled bool = true

@description('Soft delete retention days for Key Vault')
param keyVaultSoftDeleteRetentionDays int = 7

// API Management parameters
@description('Deploy API Management')
param deployApiManagement bool = true

@description('Publisher name for API Management')
param apimPublisherName string

@description('Publisher email for API Management')
param apimPublisherEmail string

@description('APIM SKU name')
@allowed([
  'Developer'
  'Premium'
])
param apimSkuName string = 'Developer'

@description('APIM SKU capacity (instance count)')
param apimSkuCapacity int = 1

// App Service Environment parameters
@description('Deploy App Service Environment')
param deployAppServiceEnvironment bool = false

// Storage Account parameters
@description('Storage accounts to deploy')
param storageAccounts array = []

// Service Bus parameters
@description('Deploy Service Bus')
param deployServiceBus bool = false

@description('Service Bus capacity units')
@allowed([
  1
  2
  4
  8
  16
])
param serviceBusCapacityUnits int = 1

@description('Service Bus queues')
param serviceBusQueues array = []

@description('Service Bus topics')
param serviceBusTopics array = []

// Reference to resource group is managed through scoped module deployment

// Get Azure Tenant ID
module currentUser 'modules/current-user.bicep' = {
  name: 'currentUser'
  scope: resourceGroup(resourceGroupName)
}

// Names module
module names 'modules/names/names.bicep' = {
  name: 'names'
  scope: resourceGroup(resourceGroupName)
  params: {
    suffix: suffix
    environment: environment
    workloadName: ''
  }
}

// Log Analytics module
module logAnalytics 'modules/log_analytics/log_analytics.bicep' = {
  name: 'logAnalytics'
  scope: resourceGroup(resourceGroupName)
  params: {
    workspaceName: names.outputs.logAnalyticsWorkspaceName
    location: location
    tags: tags
  }
}

// VNet module
module vnet 'modules/vnet/vnet.bicep' = {
  name: 'vnet'
  scope: resourceGroup(resourceGroupName)
  params: {
    vnetName: names.outputs.vnetName
    location: location
    addressSpaces: vnetAddressSpaces
    subnets: vnetSubnets
    tags: tags
  }
}

// Key Vault module
module keyVault 'modules/key_vault/key_vault.bicep' = {
  name: 'keyVault'
  scope: resourceGroup(resourceGroupName)
  params: {
    keyVaultName: names.outputs.keyVaultName
    location: location
    tenantId: currentUser.outputs.tenantId
    purgeProtectionEnabled: keyVaultPurgeProtectionEnabled
    softDeleteRetentionDays: keyVaultSoftDeleteRetentionDays
    logAnalyticsWorkspaceId: logAnalytics.outputs.workspaceId
    vnetId: vnet.outputs.vnetId
    subnetId: vnet.outputs.subnetIds['private-endpoints']
    tags: tags
  }
}

// API Management module
module apiManagement 'modules/api_management/api_management.bicep' = if (deployApiManagement) {
  name: 'apiManagement'
  scope: resourceGroup(resourceGroupName)
  params: {
    name: names.outputs.apiManagementName
    location: location
    publisherName: apimPublisherName
    publisherEmail: apimPublisherEmail
    skuName: apimSkuName
    skuCapacity: apimSkuCapacity
    subnetId: vnet.outputs.subnetIds.apim
    logAnalyticsWorkspaceId: logAnalytics.outputs.workspaceId
    enableSystemAssignedIdentity: true
    userAssignedIdentityIds: []
    tags: tags
  }
}

// App Service Environment module
module appServiceEnvironment 'modules/app_service_environment/app_service_environment.bicep' = if (deployAppServiceEnvironment) {
  name: 'appServiceEnvironment'
  scope: resourceGroup(resourceGroupName)
  params: {
    appServiceEnvironmentName: names.outputs.appServiceEnvironmentName
    location: location
    subnetId: vnet.outputs.subnetIds.ase
    tags: tags
  }
}

// Storage Account names
module storageAccountNames 'modules/names/names.bicep' = [for sa in storageAccounts: {
  name: 'storageAccountNames-${sa.name_prefix}'
  scope: resourceGroup(resourceGroupName)
  params: {
    suffix: suffix
    environment: environment
    workloadName: sa.name_prefix
  }
}]

// Storage Accounts
module storageAccountsDeployment 'modules/storage_account/storage_account.bicep' = [for (sa, i) in storageAccounts: {
  name: 'storageAccount-${sa.name_prefix}'
  scope: resourceGroup(resourceGroupName)
  params: {
    storageAccountName: storageAccountNames[i].outputs.storageAccountName
    location: location
    skuName: sa.?sku_name ?? 'Standard_LRS'
    accountKind: sa.?account_kind ?? 'StorageV2'
    accessTier: sa.?access_tier ?? 'Hot'
    minTlsVersion: sa.?min_tls_version ?? 'TLS1_2'
    allowBlobPublicAccess: sa.?allow_blob_public_access ?? false
    vnetId: vnet.outputs.vnetId
    subnetId: vnet.outputs.subnetIds['private-endpoints']
    privateEndpoints: sa.?private_endpoints ?? []
    createPrivateDnsZone: sa.?create_private_dns_zone ?? false
    blobContainers: sa.?blob_containers ?? []
    tables: sa.?tables ?? []
    queues: sa.?queues ?? []
    fileShares: sa.?file_shares ?? []
    logAnalyticsWorkspaceId: logAnalytics.outputs.workspaceId
    tags: tags
  }
}]

// Service Bus module
module serviceBus 'modules/service_bus/service_bus.bicep' = if (deployServiceBus) {
  name: 'serviceBus'
  scope: resourceGroup(resourceGroupName)
  params: {
    name: names.outputs.serviceBusNamespaceName
    location: location
    capacityUnits: serviceBusCapacityUnits
    logAnalyticsWorkspaceId: logAnalytics.outputs.workspaceId
    subnetId: vnet.outputs.subnetIds['private-endpoints']
    vnetId: vnet.outputs.vnetId
    queues: serviceBusQueues
    topics: serviceBusTopics
    tags: tags
  }
}

// Outputs
output vnetId string = vnet.outputs.vnetId
output keyVaultId string = keyVault.outputs.keyVaultId
output logAnalyticsWorkspaceId string = logAnalytics.outputs.workspaceId