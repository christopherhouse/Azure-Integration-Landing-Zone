@description('The name of the Service Bus namespace')
param name string

@description('The Azure region where the Service Bus namespace should be created')
param location string

@description('Specifies the capacity units for the Service Bus namespace (Premium tier only). Valid values are 1, 2, 4, 8, or 16.')
@allowed([
  1
  2
  4
  8
  16
])
param capacityUnits int = 1

@description('The ID of the Log Analytics workspace to which diagnostic logs will be sent')
param logAnalyticsWorkspaceId string

@description('The ID of the subnet for the private endpoint')
param subnetId string

@description('The ID of the virtual network for the private DNS zone link')
param vnetId string

@description('List of Service Bus queues to create')
param queues array = [
  {
    name: 'orders-queue'
    maxSizeInMegabytes: 1024
    defaultMessageTtl: 'P14D'  // 14 days
    maxDeliveryCount: 10
  }
  {
    name: 'notifications-queue'
    maxSizeInMegabytes: 1024
    defaultMessageTtl: 'P7D'   // 7 days
    maxDeliveryCount: 5
    requiresSession: true
    deadLetteringOnMessageExpiration: true
  }
]

@description('List of Service Bus topics to create')
param topics array = [
  {
    name: 'events'
    maxSizeInMegabytes: 1024
    defaultMessageTtl: 'P14D'  // 14 days
    subscriptions: [
      {
        name: 'all-events'
        maxDeliveryCount: 10
      }
      {
        name: 'critical-events'
        maxDeliveryCount: 20
        defaultMessageTtl: 'P7D'  // 7 days
        requiresSession: true
      }
    ]
  }
  {
    name: 'alerts'
    maxSizeInMegabytes: 1024
    subscriptions: [
      {
        name: 'system-alerts'
        maxDeliveryCount: 10
      }
      {
        name: 'security-alerts'
        maxDeliveryCount: 10
        deadLetteringOnMessageExpiration: true
        deadLetteringOnFilterEvaluationError: true
      }
    ]
  }
]

@description('Tags for resources')
param tags object = {}

resource serviceBusNamespace 'Microsoft.ServiceBus/namespaces@2022-10-01-preview' = {
  name: name
  location: location
  tags: tags
  sku: {
    name: 'Premium'
    tier: 'Premium'
    capacity: capacityUnits
  }
  properties: {
    premiumMessagingPartitions: min(capacityUnits, 4)
  }
}

resource serviceBusQueue 'Microsoft.ServiceBus/namespaces/queues@2022-10-01-preview' = [for queue in queues: {
  parent: serviceBusNamespace
  name: queue.name
  properties: {
    maxSizeInMegabytes: queue.?maxSizeInMegabytes
    defaultMessageTimeToLive: queue.?defaultMessageTtl
    maxDeliveryCount: queue.?maxDeliveryCount
    lockDuration: queue.?lockDuration
    requiresDuplicateDetection: queue.?requiresDuplicateDetection
    requiresSession: queue.?requiresSession
    deadLetteringOnMessageExpiration: queue.?deadLetteringOnMessageExpiration
  }
}]

resource serviceBusTopic 'Microsoft.ServiceBus/namespaces/topics@2022-10-01-preview' = [for topic in topics: {
  parent: serviceBusNamespace
  name: topic.name
  properties: {
    maxSizeInMegabytes: topic.?maxSizeInMegabytes
    defaultMessageTimeToLive: topic.?defaultMessageTtl
    requiresDuplicateDetection: topic.?requiresDuplicateDetection
    supportOrdering: topic.?supportOrdering
  }
}]

// Flattening subscriptions for each topic
var flattenedSubscriptions = flatten(map(topics, (topic) => map(topic.?subscriptions ?? [], (sub) => ({ topicName: topic.name, subscriptionName: sub.name, subscriptionConfig: sub }))))

resource serviceBusSubscription 'Microsoft.ServiceBus/namespaces/topics/subscriptions@2022-10-01-preview' = [for sub in flattenedSubscriptions: {
  parent: serviceBusTopic[indexOf(map(topics, t => t.name), sub.topicName)]
  name: sub.subscriptionName
  properties: {
    maxDeliveryCount: sub.subscriptionConfig.?maxDeliveryCount
    defaultMessageTimeToLive: sub.subscriptionConfig.?defaultMessageTtl
    lockDuration: sub.subscriptionConfig.?lockDuration
    deadLetteringOnMessageExpiration: sub.subscriptionConfig.?deadLetteringOnMessageExpiration
    // Using the correct property name for Bicep
    deadLetteringOnFilterEvaluationExceptions: sub.subscriptionConfig.?deadLetteringOnFilterEvaluationError
    requiresSession: sub.subscriptionConfig.?requiresSession
  }
}]

resource diagnosticSettings 'Microsoft.Insights/diagnosticSettings@2021-05-01-preview' = {
  name: 'sb-diag'
  scope: serviceBusNamespace
  properties: {
    workspaceId: logAnalyticsWorkspaceId
    logs: [
      {
        category: 'OperationalLogs'
        enabled: true
      }
      {
        category: 'VNetAndIPFilteringLogs'
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

module privateDnsZone '../privateDnsZone/privateDnsZone.bicep' = {
  name: 'sb-dns-zone'
  params: {
    zoneName: 'privatelink.servicebus.windows.net'
    linkName: 'sb-dns-link'
    vnetId: vnetId
    tags: tags
  }
}

module privateEndpoint '../privateEndpoint/privateEndpoint.bicep' = {
  name: 'sb-private-endpoint'
  params: {
    name: '${name}-pe'
    location: location
    subnetId: subnetId
    connectionName: '${name}-pe-conn'
    privateConnectionResourceId: serviceBusNamespace.id
    subresourceNames: ['namespace']
    tags: tags
  }
}

// No complex output mappings for now
// Just output the namespace information
output namespaceId string = serviceBusNamespace.id
output namespaceName string = serviceBusNamespace.name