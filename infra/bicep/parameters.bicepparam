using 'main.bicep'

param resourceGroupName = 'RG-IL-Z'
param location = 'eastus'
param suffix = 'ilz'
param environment = 'dev'
param vnetAddressSpaces = [
  '10.0.0.0/16'
]
param vnetSubnets = [
  {
    name: 'apim'
    addressPrefixes: [
      '10.0.1.0/24'
    ]
    serviceEndpoints: [
      'Microsoft.KeyVault'
      'Microsoft.Storage'
    ]
    nsg: {
      name: 'apim-nsg'
      securityRules: [
        {
          name: 'Allow_APIM_Management_Inbound'
          priority: 100
          direction: 'Inbound'
          access: 'Allow'
          protocol: 'Tcp'
          sourcePortRange: '*'
          destinationPortRange: '3443'
          sourceAddressPrefix: 'ApiManagement'
          destinationAddressPrefix: 'VirtualNetwork'
          description: 'Allow APIM management traffic'
        }
      ]
    }
  }
  {
    name: 'ase'
    addressPrefixes: [
      '10.0.2.0/24'
    ]
    delegation: {
      name: 'ase-delegation'
      serviceName: 'Microsoft.Web/hostingEnvironments'
      actions: [
        'Microsoft.Network/virtualNetworks/subnets/action'
      ]
    }
  }
  {
    name: 'private-endpoints'
    addressPrefixes: [
      '10.0.3.0/24'
    ]
  }
]
param tags = {
  Environment: 'Development'
  Project: 'Integration Landing Zone'
}
param keyVaultPurgeProtectionEnabled = true
param keyVaultSoftDeleteRetentionDays = 7
param deployApiManagement = true
param apimPublisherName = 'Example Corporation'
param apimPublisherEmail = 'admin@example.com'
param apimSkuName = 'Developer'
param apimSkuCapacity = 1
param deployAppServiceEnvironment = false
param storageAccounts = [
  {
    namePrefix: 'integration'
    skuName: 'Standard_LRS'
    accountKind: 'StorageV2'
    accessTier: 'Hot'
    privateEndpoints: [
      'blob'
      'file'
    ]
    createPrivateDnsZone: true
    blobContainers: [
      {
        name: 'archive'
        containerAccessType: 'private'
      }
      {
        name: 'incoming'
        containerAccessType: 'private'
      }
    ]
  }
]
param deployServiceBus = true
param serviceBusCapacityUnits = 1
param serviceBusQueues = [
  {
    name: 'orders-queue'
    maxSizeInMegabytes: 1024
    defaultMessageTtl: 'P14D'
    maxDeliveryCount: 10
  }
  {
    name: 'notifications-queue'
    maxSizeInMegabytes: 1024
    defaultMessageTtl: 'P7D'
    maxDeliveryCount: 5
    requiresSession: true
    deadLetteringOnMessageExpiration: true
  }
]
param serviceBusTopics = [
  {
    name: 'events'
    maxSizeInMegabytes: 1024
    defaultMessageTtl: 'P14D'
    subscriptions: [
      {
        name: 'all-events'
        maxDeliveryCount: 10
      }
      {
        name: 'critical-events'
        maxDeliveryCount: 20
        defaultMessageTtl: 'P7D'
        requiresSession: true
      }
    ]
  }
]