"""
Module for creating and managing Azure Storage Account resources.
"""

from typing import Dict, Any, Optional, List
import pulumi
from pulumi_azure_native import storage, network


class StorageAccount:
    """
    Creates a Storage Account with recommended settings for the integration landing zone.
    """

    def __init__(
        self,
        name: str,
        resource_group_name: str,
        location: str,
        vnet_id: Optional[pulumi.Input[str]] = None,
        subnet_id: Optional[pulumi.Input[str]] = None,
        sku_name: str = "Standard_LRS",
        account_kind: str = "StorageV2",
        access_tier: str = "Hot",
        min_tls_version: str = "TLS1_2",
        blob_containers: Optional[List[Dict[str, Any]]] = None,
        tables: Optional[List[Dict[str, Any]]] = None,
        queues: Optional[List[Dict[str, Any]]] = None,
        allow_blob_public_access: bool = False,
        tags: Optional[Dict[str, str]] = None,
        enable_private_endpoint: bool = True,
    ):
        """
        Initialize a new Storage Account.

        Args:
            name: The name of the Storage Account
            resource_group_name: The name of the resource group
            location: The Azure region where the account will be created
            vnet_id: The ID of the Virtual Network
            subnet_id: The ID of the Subnet to create private endpoint in
            sku_name: The SKU name (Standard_LRS, Premium_LRS, etc.)
            account_kind: The kind of storage account (StorageV2, BlobStorage, etc.)
            access_tier: The access tier (Hot or Cool)
            min_tls_version: The minimum TLS version required
            blob_containers: List of blob container configurations
            tables: List of table configurations
            queues: List of queue configurations
            allow_blob_public_access: Whether to allow public access to blobs
            tags: A dictionary of tags to assign to the account
            enable_private_endpoint: Whether to create a private endpoint
        """
        # Extract the tier and replication type from the SKU name
        sku_parts = sku_name.split("_")
        if len(sku_parts) != 2:
            raise ValueError(f"Invalid SKU name: {sku_name}. Expected format: Tier_ReplicationType")

        account_tier = sku_parts[0]
        account_replication_type = sku_parts[1]

        # Create the Storage Account
        self.account = storage.StorageAccount(
            resource_name=name,
            resource_group_name=resource_group_name,
            location=location,
            account_name=name,
            sku=storage.SkuArgs(
                name=storage.SkuName(sku_name),
            ),
            kind=account_kind,
            access_tier=access_tier,
            minimum_tls_version=min_tls_version,
            allow_blob_public_access=allow_blob_public_access,
            public_network_access=storage.PublicNetworkAccess.DISABLED,
            tags=tags or {},
        )

        # Create blob containers
        self.containers = {}
        if blob_containers:
            for container_config in blob_containers:
                container_name = container_config.get("name")

                container = storage.BlobContainer(
                    resource_name=f"{name}-{container_name}",
                    resource_group_name=resource_group_name,
                    account_name=name,
                    container_name=container_name,
                    public_access=container_config.get("container_access_type", "None"),
                    metadata=container_config.get("metadata"),
                    opts=pulumi.ResourceOptions(depends_on=[self.account]),
                )

                self.containers[container_name] = container

        # Create tables
        self.tables = {}
        if tables:
            for table_config in tables:
                table_name = table_config.get("name")

                table = storage.Table(
                    resource_name=f"{name}-{table_name}",
                    resource_group_name=resource_group_name,
                    account_name=name,
                    table_name=table_name,
                    opts=pulumi.ResourceOptions(depends_on=[self.account]),
                )

                self.tables[table_name] = table

        # Create queues
        self.queues = {}
        if queues:
            for queue_config in queues:
                queue_name = queue_config.get("name")

                queue = storage.Queue(
                    resource_name=f"{name}-{queue_name}",
                    resource_group_name=resource_group_name,
                    account_name=name,
                    queue_name=queue_name,
                    metadata=queue_config.get("metadata"),
                    opts=pulumi.ResourceOptions(depends_on=[self.account]),
                )

                self.queues[queue_name] = queue

        # Create private endpoints for different services if enabled
        self.private_endpoints = {}

        if enable_private_endpoint and vnet_id and subnet_id:
            # Define the services to create private endpoints for
            services = ["blob", "table", "queue", "file"]

            for service in services:
                self.private_endpoints[service] = network.PrivateEndpoint(
                    resource_name=f"{name}-{service}-pe",
                    resource_group_name=resource_group_name,
                    location=location,
                    private_endpoint_name=f"{name}-{service}-pe",
                    subnet=network.SubnetArgs(id=subnet_id),
                    private_link_service_connections=[
                        network.PrivateLinkServiceConnectionArgs(
                            name=f"{name}-{service}-pe-connection",
                            private_link_service_id=self.account.id,
                            group_ids=[service],
                        ),
                    ],
                    tags=tags or {},
                    opts=pulumi.ResourceOptions(depends_on=[self.account]),
                )

    @property
    def id(self) -> pulumi.Output[str]:
        """Get the ID of the Storage Account."""
        return self.account.id

    @property
    def name(self) -> pulumi.Output[str]:
        """Get the name of the Storage Account."""
        return self.account.name
