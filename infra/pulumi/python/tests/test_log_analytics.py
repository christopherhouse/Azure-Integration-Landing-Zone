"""
Unit tests for the log_analytics module.
"""

import pytest
import pulumi
from pulumi_azure_native import operationalinsights
from lz.log_analytics import LogAnalyticsWorkspace


class TestLogAnalyticsWorkspace:
    """Test cases for the LogAnalyticsWorkspace class."""

    @pytest.fixture
    def mocks(self):
        """Create pulumi mocks for testing."""

        class MockResourceMonitor(pulumi.runtime.Mocks):
            def new_resource(self, args: pulumi.runtime.MockResourceArgs):
                outputs = {
                    "id": f"/subscriptions/00000000-0000-0000-0000-000000000000/resourceGroups/{args.inputs.get('resourceGroupName')}/providers/Microsoft.OperationalInsights/workspaces/{args.inputs.get('workspaceName')}",
                    "name": args.inputs.get("workspaceName", ""),
                    "location": args.inputs.get("location", ""),
                    "sku": args.inputs.get("sku", {}),
                    "retention_in_days": args.inputs.get("retentionInDays", 30),
                }
                return [args.name + "_id", outputs]

            def call(self, args: pulumi.runtime.MockCallArgs):
                return {}

        pulumi.runtime.set_mocks(MockResourceMonitor())
        yield
        pulumi.runtime.reset_mocks()

    def test_workspace_creation(self, mocks):
        """Test that a Log Analytics workspace is created with the correct settings."""

        # Run the pulumi program
        def create_test_workspace():
            workspace = LogAnalyticsWorkspace(
                name="log-test-lz",
                resource_group_name="rg-test",
                location="eastus2",
                retention_in_days=90,
                tags={"env": "test"},
            )
            # Check that the ID is output
            pulumi.export("workspace_id", workspace.id)
            pulumi.export("workspace_name", workspace.name)

        # Execute the Pulumi program and verify the outputs
        pulumi.runtime.test_mode = True
        stack = pulumi.test.test_with_mocks(create_test_workspace, mocks)

        # Check that the workspace has the expected properties
        assert stack.outputs["workspace_name"].value == "log-test-lz"
        assert "rg-test" in stack.outputs["workspace_id"].value
