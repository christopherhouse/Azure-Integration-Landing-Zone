@description('Private endpoint name')
param name string

@description('Azure region')
param location string

@description('Subnet ID for the private endpoint')
param subnetId string

@description('Private service connection name')
param connectionName string

@description('Resource ID for the private connection')
param privateConnectionResourceId string

@description('Subresource names for the private endpoint')
param subresourceNames array

@description('Tags for resources')
param tags object = {}

@description('Optional custom network interface name')
param customNetworkInterfaceName string = 'nic-${name}'

resource privateEndpoint 'Microsoft.Network/privateEndpoints@2023-04-01' = {
  name: name
  location: location
  tags: tags
  properties: {
    subnet: {
      id: subnetId
    }
    customNetworkInterfaceName: customNetworkInterfaceName
    privateLinkServiceConnections: [
      {
        name: connectionName
        properties: {
          privateLinkServiceId: privateConnectionResourceId
          groupIds: subresourceNames
        }
      }
    ]
  }
}

output privateIpAddress string = privateEndpoint.properties.networkInterfaces[0].properties.ipConfigurations[0].properties.privateIPAddress
output id string = privateEndpoint.id