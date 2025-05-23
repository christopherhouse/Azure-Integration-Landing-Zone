"""
Module for creating and managing Azure API Management resources.
"""

from typing import Dict, Any, Optional, List
import pulumi
from pulumi_azure_native import apimanagement
from pulumi_azure_native.apimanagement import VirtualNetworkType


class ApiManagement:
    """
    Creates an API Management instance with recommended settings for the integration landing zone.
    """

    def __init__(
        self,
        name: str,
        resource_group_name: str,
        location: str,
        publisher_name: str,
        publisher_email: str,
        subnet_id: pulumi.Input[str],
        sku_name: str = "Developer",
        sku_capacity: int = 1,
        enable_system_assigned_identity: bool = True,
        user_assigned_identity_ids: Optional[List[str]] = None,
        tags: Optional[Dict[str, str]] = None,
    ):
        """
        Initialize a new API Management instance.

        Args:
            name: The name of the API Management instance
            resource_group_name: The name of the resource group
            location: The Azure region where the instance will be created
            publisher_name: The name of the API publisher
            publisher_email: The email of the API publisher
            subnet_id: The ID of the subnet for VNet integration
            sku_name: The SKU name (Developer, Basic, Standard, Premium, Consumption)
            sku_capacity: The number of units for the SKU
            enable_system_assigned_identity: Whether to enable system-assigned identity
            user_assigned_identity_ids: List of user-assigned identity IDs
            tags: A dictionary of tags to assign to the instance
        """
        # Determine identity type
        if enable_system_assigned_identity and user_assigned_identity_ids:
            identity_type = "SystemAssigned, UserAssigned"
            identity_ids = user_assigned_identity_ids
        elif enable_system_assigned_identity:
            identity_type = "SystemAssigned"
            identity_ids = None
        elif user_assigned_identity_ids:
            identity_type = "UserAssigned"
            identity_ids = user_assigned_identity_ids
        else:
            identity_type = None
            identity_ids = None

        # Create the API Management instance
        self.apim = apimanagement.ApiManagementService(
            resource_name=name,
            resource_group_name=resource_group_name,
            location=location,
            service_name=name,
            publisher_name=publisher_name,
            publisher_email=publisher_email,
            sku=apimanagement.SkuDescriptionArgs(
                name=sku_name,
                capacity=sku_capacity,
            ),
            virtual_network_type=VirtualNetworkType.INTERNAL,
            virtual_network_configuration=apimanagement.VirtualNetworkConfigurationArgs(
                subnet_id=subnet_id,
            ),
            identity=(
                apimanagement.ApiManagementServiceIdentityArgs(
                    type=identity_type,
                    user_assigned_identities=identity_ids,
                )
                if identity_type
                else None
            ),
            tags=tags or {},
        )

    @property
    def id(self) -> pulumi.Output[str]:
        """Get the ID of the API Management instance."""
        return self.apim.id

    @property
    def name(self) -> pulumi.Output[str]:
        """Get the name of the API Management instance."""
        return self.apim.name

    @property
    def gateway_url(self) -> pulumi.Output[str]:
        """Get the gateway URL of the API Management instance."""
        return self.apim.gateway_url
