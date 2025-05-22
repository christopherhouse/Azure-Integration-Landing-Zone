@description('API Management name')
param name string

@description('Resource group name')
param resourceGroupName string

@description('Azure region')
param location string

@description('Log Analytics Workspace ID for diagnostics')
param logAnalyticsWorkspaceId string

@description('Subnet ID for APIM network integration')
param subnetId string

@description('APIM SKU tier. Allowed: Developer, Premium.')
@allowed([
  'Developer'
  'Premium'
])
param skuName string

@description('APIM SKU capacity (instance count)')
param skuCapacity int

@description('Enable system assigned identity')
param enableSystemAssignedIdentity bool = false

@description('User assigned identity IDs')
param userAssignedIdentityIds array = []

@description('The publisher name for API Management')
param publisherName string

@description('The publisher email for API Management')
param publisherEmail string

@description('Tags for resources')
param tags object = {}

resource apiManagement 'Microsoft.ApiManagement/service@2023-03-01-preview' = {
  name: name
  location: location
  tags: tags
  sku: {
    name: skuName
    capacity: skuCapacity
  }
  properties: {
    publisherName: publisherName
    publisherEmail: publisherEmail
    virtualNetworkType: 'Internal'
    virtualNetworkConfiguration: {
      subnetResourceId: subnetId
    }
  }
  identity: ((enableSystemAssignedIdentity || length(userAssignedIdentityIds) > 0) ? {
    type: (enableSystemAssignedIdentity && length(userAssignedIdentityIds) > 0) ? 'SystemAssigned,UserAssigned' : (enableSystemAssignedIdentity ? 'SystemAssigned' : 'UserAssigned')
    userAssignedIdentities: length(userAssignedIdentityIds) > 0 ? reduce(userAssignedIdentityIds, {}, (result, id) => union(result, { '${id}': {} })) : null
  } : null)
}

resource diagnosticSettings 'Microsoft.Insights/diagnosticSettings@2021-05-01-preview' = {
  name: '${name}-diag'
  scope: apiManagement
  properties: {
    workspaceId: logAnalyticsWorkspaceId
    logs: [
      {
        category: 'DeveloperPortalAuditLogs'
        enabled: true
      }
      {
        category: 'GatewayLlmLogs'
        enabled: true
      }
      {
        category: 'GatewayLogs'
        enabled: true
      }
      {
        category: 'WebSocketConnectionLogs'
        enabled: true
      }
    ]
    metrics: [
      {
        category: 'AllMetrics'
        enabled: true
      }
    ]
  }
}

output id string = apiManagement.id
output name string = apiManagement.name