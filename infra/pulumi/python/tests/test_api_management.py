"""
Unit tests for the api_management module.
"""
import pytest
import pulumi
from pulumi_azure_native import apimanagement
from lz.api_management import ApiManagement


class TestApiManagement:
    """Test cases for the ApiManagement class."""
    
    @pytest.fixture
    def mocks(self):
        """Create pulumi mocks for testing."""
        class MockResourceMonitor(pulumi.runtime.Mocks):
            def new_resource(self, args: pulumi.runtime.MockResourceArgs):
                resource_type = args.type_.split(":")[1]
                
                if resource_type == "apimanagement.ApiManagementService":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.ApiManagement/service/{args.inputs.get('serviceName')}",
                        "name": args.inputs.get("serviceName", ""),
                        "location": args.inputs.get("location", ""),
                        "gateway_url": f"https://{args.inputs.get('serviceName')}.azure-api.net",
                        "publisherEmail": args.inputs.get("publisherEmail", ""),
                        "publisherName": args.inputs.get("publisherName", ""),
                        "sku": {
                            "name": args.inputs.get("sku", {}).get("name", ""),
                            "capacity": args.inputs.get("sku", {}).get("capacity", 1),
                        },
                        "virtualNetworkType": args.inputs.get("virtualNetworkType", ""),
                        "virtualNetworkConfiguration": args.inputs.get("virtualNetworkConfiguration", {}),
                    }
                else:
                    outputs = {}
                    
                return [args.name + '_id', outputs]
            
            def call(self, args: pulumi.runtime.MockCallArgs):
                return {}
        
        pulumi.runtime.set_mocks(MockResourceMonitor())
        yield
        pulumi.runtime.reset_mocks()
    
    def test_api_management_creation(self, mocks):
        """Test that an API Management instance is created with the correct settings."""
        # Run the pulumi program
        def create_test_apim():
            apim = ApiManagement(
                name="apim-test-lz",
                resource_group_name="rg-test",
                location="eastus2",
                publisher_name="Test Company",
                publisher_email="admin@example.com",
                subnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test/subnets/apim",
                sku_name="Developer",
                sku_capacity=1,
                enable_system_assigned_identity=True,
                tags={"env": "test"}
            )
            
            # Check that we can access the property getters
            pulumi.export("apim_id", apim.id)
            pulumi.export("apim_name", apim.name)
            pulumi.export("apim_gateway_url", apim.gateway_url)
        
        # Execute the Pulumi program and verify the outputs
        pulumi.runtime.test_mode = True
        stack = pulumi.test.test_with_mocks(create_test_apim, mocks)
        
        # Check the outputs
        assert "apim-test-lz" in stack.outputs["apim_id"].value
        assert stack.outputs["apim_name"].value == "apim-test-lz"
        assert stack.outputs["apim_gateway_url"].value == "https://apim-test-lz.azure-api.net"