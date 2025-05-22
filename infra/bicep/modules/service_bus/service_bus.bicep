@description('The name of the Service Bus namespace')
param name string

@description('The Azure region where the Service Bus namespace should be created')
param location string

@description('The name of the resource group in which to create the Service Bus namespace')
param resourceGroupName string

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
    maxSizeInMegabytes: contains(queue, 'maxSizeInMegabytes') ? queue.maxSizeInMegabytes : null
    defaultMessageTimeToLive: contains(queue, 'defaultMessageTtl') ? queue.defaultMessageTtl : null
    maxDeliveryCount: contains(queue, 'maxDeliveryCount') ? queue.maxDeliveryCount : null
    lockDuration: contains(queue, 'lockDuration') ? queue.lockDuration : null
    requiresDuplicateDetection: contains(queue, 'requiresDuplicateDetection') ? queue.requiresDuplicateDetection : null
    requiresSession: contains(queue, 'requiresSession') ? queue.requiresSession : null
    deadLetteringOnMessageExpiration: contains(queue, 'deadLetteringOnMessageExpiration') ? queue.deadLetteringOnMessageExpiration : null
  }
}]

resource serviceBusTopic 'Microsoft.ServiceBus/namespaces/topics@2022-10-01-preview' = [for topic in topics: {
  parent: serviceBusNamespace
  name: topic.name
  properties: {
    maxSizeInMegabytes: contains(topic, 'maxSizeInMegabytes') ? topic.maxSizeInMegabytes : null
    defaultMessageTimeToLive: contains(topic, 'defaultMessageTtl') ? topic.defaultMessageTtl : null
    requiresDuplicateDetection: contains(topic, 'requiresDuplicateDetection') ? topic.requiresDuplicateDetection : null
    supportOrdering: contains(topic, 'supportOrdering') ? topic.supportOrdering : null
  }
}]

// Flattening subscriptions for each topic
var flattenedSubscriptions = flatten(map(topics, (topic) => map(contains(topic, 'subscriptions') ? topic.subscriptions : [], (sub) => ({ topicName: topic.name, subscriptionName: sub.name, subscriptionConfig: sub }))))

resource serviceBusSubscription 'Microsoft.ServiceBus/namespaces/topics/subscriptions@2022-10-01-preview' = [for sub in flattenedSubscriptions: {
  parent: serviceBusTopic[indexOf(map(topics, t => t.name), sub.topicName)]
  name: sub.subscriptionName
  properties: {
    maxDeliveryCount: contains(sub.subscriptionConfig, 'maxDeliveryCount') ? sub.subscriptionConfig.maxDeliveryCount : null
    defaultMessageTimeToLive: contains(sub.subscriptionConfig, 'defaultMessageTtl') ? sub.subscriptionConfig.defaultMessageTtl : null
    lockDuration: contains(sub.subscriptionConfig, 'lockDuration') ? sub.subscriptionConfig.lockDuration : null
    deadLetteringOnMessageExpiration: contains(sub.subscriptionConfig, 'deadLetteringOnMessageExpiration') ? sub.subscriptionConfig.deadLetteringOnMessageExpiration : null
    // Using the correct property name for Bicep
    deadLetteringOnFilterEvaluationExceptions: contains(sub.subscriptionConfig, 'deadLetteringOnFilterEvaluationError') ? sub.subscriptionConfig.deadLetteringOnFilterEvaluationError : null
    requiresSession: contains(sub.subscriptionConfig, 'requiresSession') ? sub.subscriptionConfig.requiresSession : null
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

module privateDnsZone '../private_dns_zone/private_dns_zone.bicep' = {
  name: 'sb-dns-zone'
  params: {
    zoneName: 'privatelink.servicebus.windows.net'
    resourceGroupName: resourceGroupName
    linkName: 'sb-dns-link'
    vnetId: vnetId
    tags: tags
  }
}

module privateEndpoint '../private_endpoint/private_endpoint.bicep' = {
  name: 'sb-private-endpoint'
  params: {
    name: '${name}-pe'
    location: location
    resourceGroupName: resourceGroupName
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