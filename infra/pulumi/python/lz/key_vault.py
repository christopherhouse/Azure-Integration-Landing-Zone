"""
Module for creating and managing Azure Key Vault resources.
"""
from typing import Dict, Any, Optional, List
import pulumi
from pulumi_azure_native import keyvault, network


class KeyVault:
    """
    Creates a Key Vault with recommended security settings for the integration landing zone.
    """

    def __init__(
        self,
        name: str,
        resource_group_name: str,
        location: str,
        tenant_id: str,
        vnet_id: Optional[pulumi.Input[str]] = None,
        subnet_id: Optional[pulumi.Input[str]] = None,
        purge_protection_enabled: bool = True,
        soft_delete_retention_days: int = 90,
        sku_name: str = "standard",
        network_acls: Optional[Dict[str, Any]] = None,
        access_policies: Optional[List[Dict[str, Any]]] = None,
        tags: Optional[Dict[str, str]] = None,
        enable_private_endpoint: bool = True,
    ):
        """
        Initialize a new Key Vault.

        Args:
            name: The name of the Key Vault
            resource_group_name: The name of the resource group
            location: The Azure region where the Key Vault will be created
            tenant_id: The Azure Active Directory tenant ID
            vnet_id: The ID of the Virtual Network
            subnet_id: The ID of the Subnet to create private endpoint in
            purge_protection_enabled: Whether purge protection is enabled
            soft_delete_retention_days: Soft delete retention period in days
            sku_name: The SKU name (standard or premium)
            network_acls: Network ACLs configuration
            access_policies: List of access policy configurations
            tags: A dictionary of tags to assign to the Key Vault
            enable_private_endpoint: Whether to create a private endpoint
        """
        # Create the Key Vault
        self.key_vault = keyvault.Vault(
            resource_name=name,
            resource_group_name=resource_group_name,
            location=location,
            vault_name=name,
            properties=keyvault.VaultPropertiesArgs(
                tenant_id=tenant_id,
                enabled_for_deployment=True,
                enabled_for_disk_encryption=True,
                enabled_for_template_deployment=True,
                enable_rbac_authorization=True,
                enable_soft_delete=True,
                soft_delete_retention_in_days=soft_delete_retention_days,
                enable_purge_protection=purge_protection_enabled,
                sku=keyvault.SkuArgs(
                    family="A",
                    name=keyvault.SkuName(sku_name),
                ),
                network_acls=keyvault.NetworkRuleSetArgs(
                    bypass=keyvault.NetworkRuleBypassOptions.AZURE_SERVICES,
                    default_action=keyvault.NetworkRuleAction.DENY,
                    ip_rules=network_acls.get("ip_rules", []) if network_acls else [],
                    virtual_network_rules=network_acls.get("virtual_network_rules", []) if network_acls else [],
                ) if network_acls else None,
                access_policies=[
                    keyvault.AccessPolicyEntryArgs(
                        tenant_id=policy.get("tenant_id"),
                        object_id=policy.get("object_id"),
                        permissions=keyvault.PermissionsArgs(
                            certificates=policy.get("certificate_permissions", []),
                            keys=policy.get("key_permissions", []),
                            secrets=policy.get("secret_permissions", []),
                            storage=policy.get("storage_permissions", []),
                        ),
                    )
                    for policy in (access_policies or [])
                ],
            ),
            tags=tags or {},
        )
        
        # Create private endpoint if enabled
        if enable_private_endpoint and vnet_id and subnet_id:
            self.private_endpoint = network.PrivateEndpoint(
                resource_name=f"{name}-pe",
                resource_group_name=resource_group_name,
                location=location,
                private_endpoint_name=f"{name}-pe",
                subnet=network.SubnetArgs(id=subnet_id),
                private_link_service_connections=[
                    network.PrivateLinkServiceConnectionArgs(
                        name=f"{name}-pe-connection",
                        private_link_service_id=self.key_vault.id,
                        group_ids=["vault"],
                    ),
                ],
                tags=tags or {},
                opts=pulumi.ResourceOptions(depends_on=[self.key_vault]),
            )
    
    @property
    def id(self) -> pulumi.Output[str]:
        """Get the ID of the Key Vault."""
        return self.key_vault.id
    
    @property
    def name(self) -> pulumi.Output[str]:
        """Get the name of the Key Vault."""
        return self.key_vault.name
    
    @property
    def uri(self) -> pulumi.Output[str]:
        """Get the URI of the Key Vault."""
        return self.key_vault.properties.vault_uri