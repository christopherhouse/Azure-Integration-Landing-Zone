"""
Unit tests for the service_bus module.
"""
import pytest
import pulumi
from pulumi_azure_native import servicebus
from lz.service_bus import ServiceBus


class TestServiceBus:
    """Test cases for the ServiceBus class."""
    
    @pytest.fixture
    def mocks(self):
        """Create pulumi mocks for testing."""
        class MockResourceMonitor(pulumi.runtime.Mocks):
            def new_resource(self, args: pulumi.runtime.MockResourceArgs):
                resource_type = args.type_.split(":")[1]
                
                if resource_type == "servicebus.Namespace":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.ServiceBus/namespaces/{args.inputs.get('namespaceName')}",
                        "name": args.inputs.get("namespaceName", ""),
                        "location": args.inputs.get("location", ""),
                        "sku": args.inputs.get("sku", {}),
                    }
                elif resource_type == "servicebus.Queue":
                    namespace_name = args.inputs.get("namespaceName", "")
                    queue_name = args.inputs.get("queueName", "")
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.ServiceBus/namespaces/{namespace_name}/queues/{queue_name}",
                        "name": queue_name,
                    }
                elif resource_type == "servicebus.Topic":
                    namespace_name = args.inputs.get("namespaceName", "")
                    topic_name = args.inputs.get("topicName", "")
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.ServiceBus/namespaces/{namespace_name}/topics/{topic_name}",
                        "name": topic_name,
                    }
                elif resource_type == "servicebus.Subscription":
                    namespace_name = args.inputs.get("namespaceName", "")
                    topic_name = args.inputs.get("topicName", "")
                    subscription_name = args.inputs.get("subscriptionName", "")
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.ServiceBus/namespaces/{namespace_name}/topics/{topic_name}/subscriptions/{subscription_name}",
                        "name": subscription_name,
                    }
                elif resource_type == "network.PrivateEndpoint":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Network/privateEndpoints/{args.inputs.get('privateEndpointName')}",
                        "name": args.inputs.get("privateEndpointName", ""),
                        "location": args.inputs.get("location", ""),
                    }
                else:
                    outputs = {}
                    
                return [args.name + '_id', outputs]
            
            def call(self, args: pulumi.runtime.MockCallArgs):
                return {}
        
        pulumi.runtime.set_mocks(MockResourceMonitor())
        yield
        pulumi.runtime.reset_mocks()
    
    def test_service_bus_creation(self, mocks):
        """Test that a Service Bus namespace is created with the correct settings."""
        # Run the pulumi program
        def create_test_service_bus():
            service_bus = ServiceBus(
                name="sb-test-lz",
                resource_group_name="rg-test",
                location="eastus2",
                capacity=1,
                queues=[
                    {
                        "name": "test-queue",
                        "max_size_in_megabytes": 1024,
                        "lock_duration": "PT30S",
                        "requires_duplicate_detection": True,
                    }
                ],
                topics=[
                    {
                        "name": "test-topic",
                        "max_size_in_megabytes": 1024,
                        "requires_duplicate_detection": True,
                        "subscriptions": [
                            {
                                "name": "test-subscription",
                                "max_delivery_count": 10,
                                "dead_lettering_on_message_expiration": True,
                            }
                        ]
                    }
                ],
                vnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test",
                subnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test/subnets/private-endpoints",
                tags={"env": "test"}
            )
            
            # Check that we can access the property getters
            pulumi.export("service_bus_id", service_bus.id)
            pulumi.export("service_bus_name", service_bus.name)
        
        # Execute the Pulumi program and verify the outputs
        pulumi.runtime.test_mode = True
        stack = pulumi.test.test_with_mocks(create_test_service_bus, mocks)
        
        # Check the outputs
        assert "sb-test-lz" in stack.outputs["service_bus_id"].value
        assert stack.outputs["service_bus_name"].value == "sb-test-lz"