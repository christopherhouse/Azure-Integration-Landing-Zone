@description('DNS zone name')
param zoneName string

@description('Virtual network link name')
param linkName string

@description('Virtual network ID')
param vnetId string

@description('Tags for resources')
param tags object = {}

resource privateDnsZone 'Microsoft.Network/privateDnsZones@2020-06-01' = {
  name: zoneName
  location: 'global'
  tags: tags
}

resource privateDnsZoneVnetLink 'Microsoft.Network/privateDnsZones/virtualNetworkLinks@2020-06-01' = {
  parent: privateDnsZone
  name: linkName
  location: 'global'
  properties: {
    registrationEnabled: false
    virtualNetwork: {
      id: vnetId
    }
  }
}

output zoneName string = privateDnsZone.name
output zoneId string = privateDnsZone.id