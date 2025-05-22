"""
Module for generating consistent Azure resource names.
"""
from typing import Dict, Any, Optional
import pulumi


class NamesProvider:
    """
    A provider for generating consistent Azure resource names based on naming conventions.
    Similar to the Azure Naming Terraform module used in the Terraform implementation.
    """

    def __init__(
        self, suffix: str, environment: str, workload_name: Optional[str] = None, prefix: str = ""
    ):
        """
        Initialize the names provider with the given suffix, environment, and workload name.

        Args:
            suffix: The suffix to append to resource names
            environment: The environment (dev, test, prod)
            workload_name: Optional workload name to include in resource names
            prefix: Optional prefix to prepend to resource names
        """
        self.suffix = suffix
        self.environment = environment
        self.workload_name = workload_name if workload_name else ""
        self.prefix = prefix

    def _create_name(self, resource_type: str, max_length: int = 63) -> str:
        """
        Creates a name for a resource with the given resource type.

        Args:
            resource_type: The resource type (e.g., vnet, kv, apim)
            max_length: The maximum length for the name

        Returns:
            A string with the formatted resource name
        """
        parts = []
        
        if self.prefix:
            parts.append(self.prefix)
            
        parts.append(resource_type)
        
        if self.workload_name:
            parts.append(self.workload_name)
            
        if self.environment:
            parts.append(self.environment)
            
        if self.suffix:
            parts.append(self.suffix)
            
        name = "-".join([p for p in parts if p])
        
        # Ensure the name is not longer than max_length
        if len(name) > max_length:
            # Truncate in the middle to keep prefix and suffix intact
            name = name[:max_length]
            
        return name

    @property
    def log_analytics_workspace_name(self) -> str:
        """Generate a name for a Log Analytics workspace."""
        return self._create_name("log", 63)

    @property
    def virtual_network_name(self) -> str:
        """Generate a name for a Virtual Network."""
        return self._create_name("vnet", 64)

    @property
    def key_vault_name(self) -> str:
        """Generate a name for a Key Vault."""
        return self._create_name("kv", 24)

    @property
    def api_management_name(self) -> str:
        """Generate a name for API Management."""
        return self._create_name("apim", 50)

    @property
    def app_service_environment_name(self) -> str:
        """Generate a name for App Service Environment."""
        return self._create_name("ase", 36)

    @property
    def storage_account_name(self) -> str:
        """Generate a name for a Storage Account."""
        # Storage accounts can't have hyphens
        return self._create_name("sa", 24).replace("-", "")

    @property
    def service_bus_namespace_name(self) -> str:
        """Generate a name for a Service Bus Namespace."""
        return self._create_name("sb", 50)