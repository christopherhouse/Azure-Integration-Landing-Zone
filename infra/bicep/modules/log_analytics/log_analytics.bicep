@description('The name of the Log Analytics Workspace')
param workspaceName string

@description('The name of the resource group')
param resourceGroupName string

@description('The Azure region')
param location string

@description('A map of tags to assign to resources')
param tags object = {}

resource logAnalyticsWorkspace 'Microsoft.OperationalInsights/workspaces@2022-10-01' = {
  name: workspaceName
  location: location
  tags: tags
  properties: {
    sku: {
      name: 'PerGB2018'
    }
    retentionInDays: 30
  }
}

output workspaceId string = logAnalyticsWorkspace.id
output workspaceName string = logAnalyticsWorkspace.name