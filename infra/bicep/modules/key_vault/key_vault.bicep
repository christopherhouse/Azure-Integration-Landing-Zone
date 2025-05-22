@description('The name of the Key Vault')
param keyVaultName string

@description('Azure region')
param location string

@description('Resource group name')
param resourceGroupName string

@description('Azure AD tenant ID')
param tenantId string

@description('Enable purge protection for Key Vault')
param purgeProtectionEnabled bool = true

@description('The number of days that items should be retained for soft delete')
param softDeleteRetentionDays int = 7

@description('Log Analytics Workspace ID for diagnostics')
param logAnalyticsWorkspaceId string

@description('The ID of the Virtual Network for DNS zone linking')
param vnetId string

@description('The subnet ID for the Key Vault private endpoint')
param subnetId string

@description('Tags for resources')
param tags object = {}

resource keyVault 'Microsoft.KeyVault/vaults@2022-07-01' = {
  name: keyVaultName
  location: location
  tags: tags
  properties: {
    enableRbacAuthorization: true
    tenantId: tenantId
    sku: {
      family: 'A'
      name: 'standard'
    }
    softDeleteRetentionInDays: softDeleteRetentionDays
    enablePurgeProtection: purgeProtectionEnabled ? true : null
    publicNetworkAccess: 'Disabled'
  }
}

resource diagnosticSettings 'Microsoft.Insights/diagnosticSettings@2021-05-01-preview' = {
  name: 'kv-diag'
  scope: keyVault
  properties: {
    workspaceId: logAnalyticsWorkspaceId
    logs: [
      {
        category: 'AuditEvent'
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

module privateDnsZone '../private_dns_zone/private_dns_zone.bicep' = {
  name: 'kv-dns-zone'
  params: {
    zoneName: 'privatelink.vaultcore.azure.net'
    resourceGroupName: resourceGroupName
    linkName: 'kv-dns-link'
    vnetId: vnetId
    tags: tags
  }
}

module privateEndpoint '../private_endpoint/private_endpoint.bicep' = {
  name: 'kv-private-endpoint'
  params: {
    name: '${keyVaultName}-pe'
    location: location
    resourceGroupName: resourceGroupName
    subnetId: subnetId
    connectionName: '${keyVaultName}-pe-conn'
    privateConnectionResourceId: keyVault.id
    subresourceNames: ['vault']
    tags: tags
  }
}

output keyVaultId string = keyVault.id
output keyVaultName string = keyVault.name
output privateDnsZoneName string = privateDnsZone.outputs.zoneName
output privateEndpointIp string = privateEndpoint.outputs.privateIpAddress