"""
Module for creating and managing Azure Log Analytics resources.
"""

from typing import Optional, Dict, Any
import pulumi
from pulumi_azure_native import operationalinsights


class LogAnalyticsWorkspace:
    """
    Creates a Log Analytics workspace with recommended settings for the integration landing zone.
    """

    def __init__(
        self,
        name: str,
        resource_group_name: str,
        location: str,
        sku: str = "PerGB2018",
        retention_in_days: int = 30,
        daily_quota_gb: Optional[float] = None,
        tags: Optional[Dict[str, str]] = None,
    ):
        """
        Initialize a new Log Analytics workspace.

        Args:
            name: The name of the Log Analytics workspace
            resource_group_name: The name of the resource group
            location: The Azure region where the workspace will be created
            sku: The SKU of the workspace (default: PerGB2018)
            retention_in_days: The retention period in days (default: 30)
            daily_quota_gb: The daily data cap in GB (optional)
            tags: A dictionary of tags to assign to the workspace
        """
        self.workspace = operationalinsights.Workspace(
            resource_name=name,
            resource_group_name=resource_group_name,
            location=location,
            sku=operationalinsights.WorkspaceSkuArgs(name=sku),
            retention_in_days=retention_in_days,
            workspace_name=name,
            tags=tags or {},
        )

        if daily_quota_gb is not None:
            # Set daily quota if specified
            self.workspace_settings = operationalinsights.WorkspaceDailyQuota(
                resource_name=f"{name}-quota",
                daily_quota_gb=daily_quota_gb,
                resource_group_name=resource_group_name,
                workspace_name=name,
                opts=pulumi.ResourceOptions(depends_on=[self.workspace]),
            )

    @property
    def id(self) -> pulumi.Output[str]:
        """Get the ID of the workspace."""
        return self.workspace.id

    @property
    def name(self) -> pulumi.Output[str]:
        """Get the name of the workspace."""
        return self.workspace.name
