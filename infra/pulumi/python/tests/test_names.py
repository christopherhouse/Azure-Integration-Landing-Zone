"""
Unit tests for the names module.
"""

import pytest
from lz.names import NamesProvider


class TestNamesProvider:
    """Test cases for the NamesProvider class."""

    def test_basic_name_generation(self):
        """Test the basic name generation functionality."""
        names = NamesProvider(suffix="lz", environment="dev")

        assert names.log_analytics_workspace_name == "log-dev-lz"
        assert names.virtual_network_name == "vnet-dev-lz"
        assert names.key_vault_name == "kv-dev-lz"
        assert names.api_management_name == "apim-dev-lz"
        assert names.app_service_environment_name == "ase-dev-lz"
        assert names.storage_account_name == "sadevlz"
        assert names.service_bus_namespace_name == "sb-dev-lz"

    def test_with_workload_name(self):
        """Test name generation with a workload name."""
        names = NamesProvider(suffix="lz", environment="prod", workload_name="api")

        assert names.log_analytics_workspace_name == "log-api-prod-lz"
        assert names.storage_account_name == "saapiprodlz"

    def test_with_prefix(self):
        """Test name generation with a prefix."""
        names = NamesProvider(
            prefix="contoso", suffix="lz", environment="test", workload_name="web"
        )

        assert names.virtual_network_name == "contoso-vnet-web-test-lz"
        assert names.key_vault_name == "contoso-kv-web-test-lz"

    def test_name_length_limits(self):
        """Test that names are truncated to respect length limits."""
        # Create a provider with very long values
        names = NamesProvider(
            prefix="contoso-very-long-prefix",
            suffix="extremely-long-suffix-that-will-be-truncated",
            environment="production",
            workload_name="web-application-workload",
        )

        # Key vault has a strict 24 character limit
        assert len(names.key_vault_name) <= 24
        assert len(names.storage_account_name) <= 24
