"""
Unit tests for the vnet module.
"""

import pytest
import pulumi
from pulumi_azure_native import network
from lz.vnet import VirtualNetwork


class TestVirtualNetwork:
    """Test cases for the VirtualNetwork class."""

    @pytest.fixture
    def mocks(self):
        """Create pulumi mocks for testing."""

        class MockResourceMonitor(pulumi.runtime.Mocks):
            def new_resource(self, args: pulumi.runtime.MockResourceArgs):
                # Extract the resource name from args
                resource_type = args.type_.split(":")[1]
                if resource_type == "network.VirtualNetwork":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Network/virtualNetworks/{args.inputs.get('virtualNetworkName')}",
                        "name": args.inputs.get("virtualNetworkName", ""),
                        "addressSpace": {
                            "addressPrefixes": args.inputs.get("addressSpace", {}).get(
                                "addressPrefixes", []
                            )
                        },
                        "location": args.inputs.get("location", ""),
                    }
                elif resource_type == "network.Subnet":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Network/virtualNetworks/{args.inputs.get('virtualNetworkName')}/subnets/{args.inputs.get('subnetName')}",
                        "name": args.inputs.get("subnetName", ""),
                        "addressPrefix": args.inputs.get("addressPrefix", ""),
                        "addressPrefixes": args.inputs.get("addressPrefixes", []),
                    }
                else:
                    outputs = {}
                return [args.name + "_id", outputs]

            def call(self, args: pulumi.runtime.MockCallArgs):
                return {}

        pulumi.runtime.set_mocks(MockResourceMonitor())
        yield
        # Remove reset_mocks call
        # pulumi.runtime.reset_mocks()

    def test_vnet_creation(self, mocks):
        """Test that a virtual network is created with the correct settings."""

        # Run the pulumi program
        def create_test_vnet():
            vnet = VirtualNetwork(
                name="vnet-test-lz",
                resource_group_name="rg-test",
                location="eastus2",
                address_spaces=["10.10.0.0/16"],
                subnets=[
                    {
                        "name": "default",
                        "address_prefixes": ["10.10.0.0/24"],
                    },
                    {
                        "name": "ase",
                        "address_prefixes": ["10.10.1.0/24"],
                        "delegation": {
                            "name": "ase-delegation",
                            "service_name": "Microsoft.Web/hostingEnvironments",
                            "actions": ["Microsoft.Network/virtualNetworks/subnets/action"],
                        },
                    },
                ],
                tags={"env": "test"},
            )

            # Check that we can get subnet IDs
            pulumi.export("vnet_id", vnet.id)
            pulumi.export("default_subnet_id", vnet.get_subnet_id("default"))
            pulumi.export("ase_subnet_id", vnet.get_subnet_id("ase"))
            return vnet

        # Execute the Pulumi program and verify the outputs
        pulumi.runtime.test_mode = True
        outputs = pulumi.runtime.test(create_test_vnet)

        # Check the outputs
        assert "vnet-test-lz" in outputs.get("vnet_id")
        assert "default" in outputs.get("default_subnet_id")
        assert "ase" in outputs.get("ase_subnet_id")
