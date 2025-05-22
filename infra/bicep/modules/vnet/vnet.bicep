@description('Name of the virtual network')
param vnetName string

@description('Azure region')
param location string

@description('Resource group name')
param resourceGroupName string

@description('Array of address spaces for the VNet')
param addressSpaces array

@description('Array of subnet objects')
param subnets array

@description('Tags for resources')
param tags object = {}

resource virtualNetwork 'Microsoft.Network/virtualNetworks@2023-04-01' = {
  name: vnetName
  location: location
  tags: tags
  properties: {
    addressSpace: {
      addressPrefixes: addressSpaces
    }
  }
}

resource subnet 'Microsoft.Network/virtualNetworks/subnets@2023-04-01' = [for (sub, i) in subnets: {
  parent: virtualNetwork
  name: sub.name
  properties: {
    addressPrefix: sub.addressPrefixes[0]
    addressPrefixes: sub.addressPrefixes
    serviceEndpoints: contains(sub, 'serviceEndpoints') ? sub.serviceEndpoints : null
    delegations: contains(sub, 'delegation') ? [
      {
        name: sub.delegation.name
        properties: {
          serviceName: sub.delegation.serviceName
        }
      }
    ] : null
  }
}]

resource networkSecurityGroup 'Microsoft.Network/networkSecurityGroups@2023-04-01' = [for sub in subnets: if(contains(sub, 'nsg')) {
  name: sub.nsg.name
  location: location
  tags: tags
  properties: {
    securityRules: [for rule in sub.nsg.securityRules: {
      name: rule.name
      properties: {
        priority: rule.priority
        direction: rule.direction
        access: rule.access
        protocol: rule.protocol
        sourcePortRange: contains(rule, 'sourcePortRange') ? rule.sourcePortRange : null
        sourcePortRanges: contains(rule, 'sourcePortRanges') ? rule.sourcePortRanges : null
        destinationPortRange: contains(rule, 'destinationPortRange') ? rule.destinationPortRange : null
        destinationPortRanges: contains(rule, 'destinationPortRanges') ? rule.destinationPortRanges : null
        sourceAddressPrefix: contains(rule, 'sourceAddressPrefix') ? rule.sourceAddressPrefix : null
        sourceAddressPrefixes: contains(rule, 'sourceAddressPrefixes') ? rule.sourceAddressPrefixes : null
        destinationAddressPrefix: contains(rule, 'destinationAddressPrefix') ? rule.destinationAddressPrefix : null
        destinationAddressPrefixes: contains(rule, 'destinationAddressPrefixes') ? rule.destinationAddressPrefixes : null
        description: rule.description
      }
    }]
  }
}]

// This is a workaround for the dependency issue - we need to create a mapping array of subnet names to NSG names
var subnetNsgMap = [for (sub, i) in subnets: {
  subnetName: sub.name
  hasNsg: contains(sub, 'nsg')
  nsgName: contains(sub, 'nsg') ? sub.nsg.name : ''
}]

// Subnet NSG associations
resource subnetNsgAssociation 'Microsoft.Network/virtualNetworks/subnets@2023-04-01' = [for (mapping, i) in subnetNsgMap: if(mapping.hasNsg) {
  parent: virtualNetwork
  name: mapping.subnetName
  properties: {
    addressPrefix: subnet[i].properties.addressPrefix
    addressPrefixes: subnet[i].properties.addressPrefixes
    serviceEndpoints: subnet[i].properties.serviceEndpoints
    delegations: subnet[i].properties.delegations
    networkSecurityGroup: {
      id: resourceId('Microsoft.Network/networkSecurityGroups', mapping.nsgName)
    }
  }
  dependsOn: [
    subnet[i]
    networkSecurityGroup
  ]
}]

// Route tables
resource routeTable 'Microsoft.Network/routeTables@2023-04-01' = [for sub in subnets: if(contains(sub, 'routeTable')) {
  name: sub.routeTable.name
  location: location
  tags: tags
  properties: {
    routes: [for route in sub.routeTable.routes: {
      name: route.name
      properties: {
        addressPrefix: route.addressPrefix
        nextHopType: route.nextHopType
        nextHopIpAddress: contains(route, 'nextHopIpAddress') ? route.nextHopIpAddress : null
      }
    }]
  }
}]

// This is a workaround for the dependency issue - we need to create a mapping array of subnet names to route table names
var subnetRtMap = [for (sub, i) in subnets: {
  subnetName: sub.name
  hasRt: contains(sub, 'routeTable')
  rtName: contains(sub, 'routeTable') ? sub.routeTable.name : ''
}]

// Subnet route table associations
resource subnetRtAssociation 'Microsoft.Network/virtualNetworks/subnets@2023-04-01' = [for (mapping, i) in subnetRtMap: if(mapping.hasRt) {
  parent: virtualNetwork
  name: mapping.subnetName
  properties: {
    addressPrefix: subnet[i].properties.addressPrefix
    addressPrefixes: subnet[i].properties.addressPrefixes
    serviceEndpoints: subnet[i].properties.serviceEndpoints
    delegations: subnet[i].properties.delegations
    networkSecurityGroup: contains(subnetNsgMap[i], 'hasNsg') && subnetNsgMap[i].hasNsg ? {
      id: resourceId('Microsoft.Network/networkSecurityGroups', subnetNsgMap[i].nsgName)
    } : null
    routeTable: {
      id: resourceId('Microsoft.Network/routeTables', mapping.rtName)
    }
  }
  dependsOn: [
    subnet[i]
    routeTable
    subnetNsgAssociation
  ]
}]

// Outputs
output vnetId string = virtualNetwork.id
// Simple output with just the subnet names
output subnetIds object = {
  'apim': '${virtualNetwork.id}/subnets/apim' 
  'private-endpoints': '${virtualNetwork.id}/subnets/private-endpoints'
  'ase': '${virtualNetwork.id}/subnets/ase'
}