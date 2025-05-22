@description('App Service Environment name')
param appServiceEnvironmentName string

@description('Azure region')
param location string

@description('Subnet ID for the App Service Environment')
param subnetId string

@description('Tags for resources')
param tags object = {}

resource appServiceEnvironment 'Microsoft.Web/hostingEnvironments@2022-09-01' = {
  name: appServiceEnvironmentName
  location: location
  tags: tags
  kind: 'ASEv3'
  properties: {
    virtualNetwork: {
      id: subnetId
    }
    zoneRedundant: false
  }
}

output id string = appServiceEnvironment.id
output name string = appServiceEnvironment.name