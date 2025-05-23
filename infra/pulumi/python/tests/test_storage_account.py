"""
Unit tests for the storage_account module.
"""

import pytest
import pulumi
from pulumi_azure_native import storage
from lz.storage_account import StorageAccount


class TestStorageAccount:
    """Test cases for the StorageAccount class."""

    @pytest.fixture
    def mocks(self):
        """Create pulumi mocks for testing."""

        class MockResourceMonitor(pulumi.runtime.Mocks):
            def new_resource(self, args: pulumi.runtime.MockResourceArgs):
                resource_type = args.type_.split(":")[1]

                if resource_type == "storage.StorageAccount":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Storage/storageAccounts/{args.inputs.get('accountName')}",
                        "name": args.inputs.get("accountName", ""),
                        "location": args.inputs.get("location", ""),
                        "kind": args.inputs.get("kind", ""),
                        "sku": args.inputs.get("sku", {}),
                        "accessTier": args.inputs.get("accessTier", ""),
                        "minimumTlsVersion": args.inputs.get("minimumTlsVersion", ""),
                        "allowBlobPublicAccess": args.inputs.get("allowBlobPublicAccess", False),
                        "publicNetworkAccess": args.inputs.get("publicNetworkAccess", ""),
                    }
                elif resource_type == "storage.BlobContainer":
                    account_name = args.inputs.get("accountName", "")
                    container_name = args.inputs.get("containerName", "")
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Storage/storageAccounts/{account_name}/blobServices/default/containers/{container_name}",
                        "name": container_name,
                    }
                elif resource_type == "storage.Table":
                    account_name = args.inputs.get("accountName", "")
                    table_name = args.inputs.get("tableName", "")
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Storage/storageAccounts/{account_name}/tableServices/default/tables/{table_name}",
                        "name": table_name,
                    }
                elif resource_type == "storage.Queue":
                    account_name = args.inputs.get("accountName", "")
                    queue_name = args.inputs.get("queueName", "")
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Storage/storageAccounts/{account_name}/queueServices/default/queues/{queue_name}",
                        "name": queue_name,
                    }
                elif resource_type == "network.PrivateEndpoint":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.Network/privateEndpoints/{args.inputs.get('privateEndpointName')}",
                        "name": args.inputs.get("privateEndpointName", ""),
                        "location": args.inputs.get("location", ""),
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

    def test_storage_account_creation(self, mocks):
        """Test that a Storage Account is created with the correct settings."""

        # Run the pulumi program
        def create_test_storage_account():
            storage_account = StorageAccount(
                name="satest",
                resource_group_name="rg-test",
                location="eastus2",
                sku_name="Standard_LRS",
                account_kind="StorageV2",
                access_tier="Hot",
                min_tls_version="TLS1_2",
                blob_containers=[
                    {
                        "name": "test-container",
                        "container_access_type": "None",
                    }
                ],
                tables=[
                    {
                        "name": "testtable",
                    }
                ],
                queues=[
                    {
                        "name": "test-queue",
                    }
                ],
                vnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test",
                subnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test/subnets/private-endpoints",
                tags={"env": "test"},
                allow_blob_public_access=False,
            )

            # Check that we can access the property getters
            pulumi.export("storage_account_id", storage_account.id)
            pulumi.export("storage_account_name", storage_account.name)
            return storage_account

        # Execute the Pulumi program and verify the outputs
        pulumi.runtime.test_mode = True
        outputs = pulumi.runtime.test(create_test_storage_account)

        # Check the outputs
        assert "satest" in outputs.get("storage_account_id")
        assert outputs.get("storage_account_name") == "satest"
