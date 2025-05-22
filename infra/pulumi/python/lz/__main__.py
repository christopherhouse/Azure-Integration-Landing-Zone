"""
Main entry point for the Azure Integration Landing Zone Pulumi program.
"""

import pulumi
from pulumi_azure_native import resources
from pulumi_azure_native import authorization

from lz.names import NamesProvider
from lz.log_analytics import LogAnalyticsWorkspace
from lz.vnet import VirtualNetwork
from lz.key_vault import KeyVault
from lz.api_management import ApiManagement
from lz.service_bus import ServiceBus
from lz.storage_account import StorageAccount

# Get configuration
config = pulumi.Config()
subscription_id = config.require("subscriptionId")
resource_group_name = config.require("resourceGroupName")
location = config.get("location") or "eastus2"
suffix = config.require("suffix")
environment = config.require("environment")
log_analytics_workspace_name = config.get("logAnalyticsWorkspaceName")
vnet_address_spaces = config.get_object("vnetAddressSpaces") or ["10.10.0.0/16"]
deploy_api_management = config.get_bool("deployApiManagement") or False
deploy_service_bus = config.get_bool("deployServiceBus") or False

# Define default subnet configuration
vnet_subnets = config.get_object("vnetSubnets") or [
    {
        "name": "ase",
        "address_prefixes": ["10.10.1.0/24"],
        "delegation": {
            "name": "ase-delegation",
            "service_name": "Microsoft.Web/hostingEnvironments",
            "actions": ["Microsoft.Network/virtualNetworks/subnets/action"],
        },
        "service_endpoints": [],
    },
    {"name": "private-endpoints", "address_prefixes": ["10.10.2.0/24"], "service_endpoints": []},
]

# Add APIM subnet if needed
if deploy_api_management:
    vnet_subnets.append(
        {
            "name": "apim",
            "address_prefixes": ["10.10.3.0/24"],
            "service_endpoints": ["Microsoft.ApiManagement"],
        }
    )

# Get the current client config to get the tenant ID
current_client = authorization.get_client_config()

# Create a resource group if it doesn't exist
resource_group = resources.ResourceGroup.get(
    "resource_group",
    resource_group_id=f"/subscriptions/{subscription_id}/resourceGroups/{resource_group_name}",
)

# Create a names provider
names = NamesProvider(suffix=suffix, environment=environment)

# Create Log Analytics workspace
log_analytics = LogAnalyticsWorkspace(
    name=log_analytics_workspace_name or names.log_analytics_workspace_name,
    resource_group_name=resource_group_name,
    location=location,
    retention_in_days=30,
    tags={"environment": environment},
)

# Create Virtual Network
vnet = VirtualNetwork(
    name=names.virtual_network_name,
    resource_group_name=resource_group_name,
    location=location,
    address_spaces=vnet_address_spaces,
    subnets=vnet_subnets,
    tags={"environment": environment},
)

# Create Key Vault
key_vault = KeyVault(
    name=names.key_vault_name,
    resource_group_name=resource_group_name,
    location=location,
    tenant_id=current_client.tenant_id,
    vnet_id=vnet.id,
    subnet_id=vnet.get_subnet_id("private-endpoints"),
    purge_protection_enabled=True,
    soft_delete_retention_days=90,
    tags={"environment": environment},
)

# Create API Management if enabled
apim = None
if deploy_api_management:
    apim = ApiManagement(
        name=names.api_management_name,
        resource_group_name=resource_group_name,
        location=location,
        publisher_name=config.require("apimPublisherName"),
        publisher_email=config.require("apimPublisherEmail"),
        subnet_id=vnet.get_subnet_id("apim"),
        sku_name=config.get("apimSkuName") or "Developer",
        sku_capacity=config.get_int("apimSkuCapacity") or 1,
        enable_system_assigned_identity=True,
        tags={"environment": environment},
    )

# Create Service Bus if enabled
service_bus = None
if deploy_service_bus:
    service_bus = ServiceBus(
        name=names.service_bus_namespace_name,
        resource_group_name=resource_group_name,
        location=location,
        vnet_id=vnet.id,
        subnet_id=vnet.get_subnet_id("private-endpoints"),
        capacity=config.get_int("serviceBusCapacityUnits") or 1,
        tags={"environment": environment},
    )

# Export outputs
pulumi.export("resource_group_name", resource_group.name)
pulumi.export("log_analytics_workspace_id", log_analytics.id)
pulumi.export("vnet_id", vnet.id)
pulumi.export("key_vault_id", key_vault.id)
pulumi.export("key_vault_uri", key_vault.uri)

if apim:
    pulumi.export("api_management_id", apim.id)
    pulumi.export("api_management_gateway_url", apim.gateway_url)

if service_bus:
    pulumi.export("service_bus_namespace_id", service_bus.id)
