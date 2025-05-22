@description('Azure region for the storage account')
param location string

@description('Resource group name')
param resourceGroupName string

@description('Storage account name')
param storageAccountName string

@description('Storage account SKU')
param skuName string = 'Standard_LRS'

@description('Storage account kind')
param accountKind string = 'StorageV2'

@description('Access tier for blob storage')
param accessTier string = 'Hot'

@description('Minimum TLS version')
param minTlsVersion string = 'TLS1_2'

@description('Allow public access to blobs')
param allowBlobPublicAccess bool = false

@description('VNet ID for private endpoints')
param vnetId string

@description('Subnet ID for private endpoints')
param subnetId string

@description('List of storage services to create private endpoints for (e.g. ["blob", "file"])')
param privateEndpoints array = []

@description('Whether to create and link private DNS zones for private endpoints')
param createPrivateDnsZone bool = false

@description('List of blob containers to create')
param blobContainers array = []

@description('List of tables to create')
param tables array = []

@description('List of queues to create')
param queues array = []

@description('List of file shares to create')
param fileShares array = []

@description('Resource ID of the Log Analytics workspace for diagnostics')
param logAnalyticsWorkspaceId string

@description('Tags for resources')
param tags object = {}

// Get the first part of SKU (Standard or Premium)
var skuTier = split(skuName, '_')[0]
// Get the second part of SKU (LRS, ZRS, GRS, etc.)
var skuReplication = split(skuName, '_')[1]

// Storage Account
resource storageAccount 'Microsoft.Storage/storageAccounts@2023-01-01' = {
  name: storageAccountName
  location: location
  tags: tags
  sku: {
    name: skuName
  }
  kind: accountKind
  properties: {
    accessTier: accessTier
    minimumTlsVersion: minTlsVersion
    allowBlobPublicAccess: allowBlobPublicAccess
    publicNetworkAccess: 'Disabled'
  }
}

// Blob Containers
resource blobContainer 'Microsoft.Storage/storageAccounts/blobServices/containers@2023-01-01' = [for container in blobContainers: {
  name: '${storageAccount.name}/default/${container.name}'
  properties: {
    publicAccess: contains(container, 'containerAccessType') ? container.containerAccessType : 'None'
    metadata: contains(container, 'metadata') ? container.metadata : null
  }
}]

// Tables
resource storageTable 'Microsoft.Storage/storageAccounts/tableServices/tables@2023-01-01' = [for table in tables: {
  name: '${storageAccount.name}/default/${table.name}'
}]

// Queues
resource storageQueue 'Microsoft.Storage/storageAccounts/queueServices/queues@2023-01-01' = [for queue in queues: {
  name: '${storageAccount.name}/default/${queue.name}'
  properties: {
    metadata: contains(queue, 'metadata') ? queue.metadata : null
  }
}]

// File Shares
resource fileShare 'Microsoft.Storage/storageAccounts/fileServices/shares@2023-01-01' = [for share in fileShares: {
  name: '${storageAccount.name}/default/${share.name}'
  properties: {
    shareQuota: contains(share, 'quota') ? share.quota : null
    metadata: contains(share, 'metadata') ? share.metadata : null
  }
}]

// Private DNS Zones for storage services
resource privateDnsZone 'Microsoft.Network/privateDnsZones@2020-06-01' = [for endpoint in privateEndpoints: if (createPrivateDnsZone) {
  name: 'privatelink.${endpoint}.core.windows.net'
  location: 'global'
  tags: tags
}]

// Private DNS Zone VNet Links
resource privateDnsZoneVNetLink 'Microsoft.Network/privateDnsZones/virtualNetworkLinks@2020-06-01' = [for (endpoint, i) in privateEndpoints: if (createPrivateDnsZone) {
  name: '${privateDnsZone[i].name}/pdzvnetlink-${endpoint}'
  location: 'global'
  properties: {
    registrationEnabled: false
    virtualNetwork: {
      id: vnetId
    }
  }
}]

// Private Endpoints
resource privateEndpoint 'Microsoft.Network/privateEndpoints@2023-04-01' = [for endpoint in privateEndpoints: {
  name: 'pe-${storageAccount.name}-${endpoint}'
  location: location
  tags: tags
  properties: {
    subnet: {
      id: subnetId
    }
    customNetworkInterfaceName: 'pe-${storageAccount.name}-${endpoint}-nic'
    privateLinkServiceConnections: [
      {
        name: 'psc-${storageAccount.name}-${endpoint}'
        properties: {
          privateLinkServiceId: storageAccount.id
          groupIds: [endpoint]
        }
      }
    ]
  }
}]

// Simplify to just one diagnostic setting to avoid validation errors
resource blobDiagnostics 'Microsoft.Insights/diagnosticSettings@2021-05-01-preview' = {
  name: 'diag-storage'
  scope: storageAccount
  properties: {
    workspaceId: logAnalyticsWorkspaceId
    metrics: [
      {
        category: 'AllMetrics'
        enabled: true
      }
    ]
  }
}

// References to child services
// Commented out to avoid build errors

output storageAccountName string = storageAccount.name
output storageAccountId string = storageAccount.id