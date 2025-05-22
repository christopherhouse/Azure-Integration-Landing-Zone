"""
Unit tests for the key_vault module.
"""

import pytest
import pulumi
from pulumi_azure_native import keyvault, network
from lz.key_vault import KeyVault


class TestKeyVault:
    """Test cases for the KeyVault class."""

    @pytest.fixture
    def mocks(self):
        """Create pulumi mocks for testing."""

        class MockResourceMonitor(pulumi.runtime.Mocks):
            def new_resource(self, args: pulumi.runtime.MockResourceArgs):
                resource_type = args.type_.split(":")[1]

                if resource_type == "keyvault.Vault":
                    outputs = {
                        "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.KeyVault/vaults/{args.inputs.get('vaultName')}",
                        "name": args.inputs.get("vaultName", ""),
                        "location": args.inputs.get("location", ""),
                        "properties": {
                            "vaultUri": f"https://{args.inputs.get('vaultName')}.vault.azure.net/",
                            "tenantId": args.inputs.get("properties", {}).get("tenantId", ""),
                            "enableRbacAuthorization": args.inputs.get("properties", {}).get(
                                "enableRbacAuthorization", False
                            ),
                            "enableSoftDelete": args.inputs.get("properties", {}).get(
                                "enableSoftDelete", False
                            ),
                            "softDeleteRetentionInDays": args.inputs.get("properties", {}).get(
                                "softDeleteRetentionInDays", 90
                            ),
                            "enablePurgeProtection": args.inputs.get("properties", {}).get(
                                "enablePurgeProtection", False
                            ),
                        },
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
        pulumi.runtime.reset_mocks()

    def test_key_vault_creation(self, mocks):
        """Test that a Key Vault is created with the correct settings."""

        # Run the pulumi program
        def create_test_key_vault():
            kv = KeyVault(
                name="kv-test-lz",
                resource_group_name="rg-test",
                location="eastus2",
                tenant_id="00000000-0000-0000-0000-000000000000",
                vnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test",
                subnet_id="/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/rg-test/providers/Microsoft.Network/virtualNetworks/vnet-test/subnets/private-endpoints",
                purge_protection_enabled=True,
                soft_delete_retention_days=90,
                tags={"env": "test"},
            )

            # Check that we can access the property getters
            pulumi.export("key_vault_id", kv.id)
            pulumi.export("key_vault_name", kv.name)
            pulumi.export("key_vault_uri", kv.uri)

        # Execute the Pulumi program and verify the outputs
        pulumi.runtime.test_mode = True
        stack = pulumi.test.test_with_mocks(create_test_key_vault, mocks)

        # Check the outputs
        assert "kv-test-lz" in stack.outputs["key_vault_id"].value
        assert stack.outputs["key_vault_name"].value == "kv-test-lz"
        assert stack.outputs["key_vault_uri"].value == "https://kv-test-lz.vault.azure.net/"
