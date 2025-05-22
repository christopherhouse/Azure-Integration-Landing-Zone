"""
Module for creating and managing Azure Virtual Network resources.
"""
from typing import List, Dict, Any, Optional, Union
import pulumi
from pulumi_azure_native import network


class VirtualNetwork:
    """
    Creates a Virtual Network with subnets for the integration landing zone.
    """

    def __init__(
        self,
        name: str,
        resource_group_name: str,
        location: str,
        address_spaces: List[str],
        subnets: Optional[List[Dict[str, Any]]] = None,
        dns_servers: Optional[List[str]] = None,
        tags: Optional[Dict[str, str]] = None,
    ):
        """
        Initialize a new Virtual Network.

        Args:
            name: The name of the Virtual Network
            resource_group_name: The name of the resource group
            location: The Azure region where the VNet will be created
            address_spaces: List of address spaces to use for the VNet
            subnets: List of subnet configurations
            dns_servers: List of DNS servers
            tags: A dictionary of tags to assign to the VNet
        """
        self.subnet_resources = {}
        
        # Create the Virtual Network
        self.vnet = network.VirtualNetwork(
            resource_name=name,
            resource_group_name=resource_group_name,
            location=location,
            virtual_network_name=name,
            address_space=network.AddressSpaceArgs(
                address_prefixes=address_spaces,
            ),
            dhcp_options=network.DhcpOptionsArgs(
                dns_servers=dns_servers,
            ) if dns_servers else None,
            tags=tags or {},
        )
        
        # Create subnets
        if subnets:
            for subnet in subnets:
                subnet_name = subnet.get("name")
                address_prefixes = subnet.get("address_prefixes", [])
                
                subnet_args = {
                    "resource_name": f"{name}-{subnet_name}",
                    "resource_group_name": resource_group_name,
                    "virtual_network_name": name,
                    "subnet_name": subnet_name,
                    "address_prefix": address_prefixes[0] if len(address_prefixes) == 1 else None,
                    "address_prefixes": address_prefixes if len(address_prefixes) > 1 else None,
                    "service_endpoints": subnet.get("service_endpoints", []),
                    "opts": pulumi.ResourceOptions(depends_on=[self.vnet]),
                }
                
                # Add delegation if specified
                delegation = subnet.get("delegation")
                if delegation:
                    subnet_args["delegations"] = [
                        network.DelegationArgs(
                            name=delegation.get("name"),
                            service_name=delegation.get("service_name"),
                            service_delegation=network.ServiceDelegationArgs(
                                name=delegation.get("service_name"),
                                actions=delegation.get("actions", []),
                            ),
                        )
                    ]
                
                # Create the subnet
                self.subnet_resources[subnet_name] = network.Subnet(**subnet_args)
    
    @property
    def id(self) -> pulumi.Output[str]:
        """Get the ID of the Virtual Network."""
        return self.vnet.id
    
    @property
    def name(self) -> pulumi.Output[str]:
        """Get the name of the Virtual Network."""
        return self.vnet.name
    
    def get_subnet_id(self, subnet_name: str) -> pulumi.Output[str]:
        """
        Get the ID of a subnet by name.
        
        Args:
            subnet_name: The name of the subnet
            
        Returns:
            The ID of the subnet
        """
        if subnet_name not in self.subnet_resources:
            raise ValueError(f"Subnet '{subnet_name}' does not exist")
        
        return self.subnet_resources[subnet_name].id