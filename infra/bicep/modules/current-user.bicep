@description('Get current user/SPN tenant ID')

// No resources, just outputs to expose the tenant ID information
output tenantId string = subscription().tenantId