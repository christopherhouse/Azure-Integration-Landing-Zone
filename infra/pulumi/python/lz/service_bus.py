"""
Module for creating and managing Azure Service Bus resources.
"""

from typing import Dict, Any, Optional, List
import pulumi
from pulumi_azure_native import servicebus, network


class ServiceBus:
    """
    Creates a Service Bus namespace with queues, topics, and subscriptions for the integration landing zone.
    """

    def __init__(
        self,
        name: str,
        resource_group_name: str,
        location: str,
        vnet_id: Optional[pulumi.Input[str]] = None,
        subnet_id: Optional[pulumi.Input[str]] = None,
        capacity: int = 1,
        queues: Optional[List[Dict[str, Any]]] = None,
        topics: Optional[List[Dict[str, Any]]] = None,
        tags: Optional[Dict[str, str]] = None,
        enable_private_endpoint: bool = True,
    ):
        """
        Initialize a new Service Bus namespace.

        Args:
            name: The name of the Service Bus namespace
            resource_group_name: The name of the resource group
            location: The Azure region where the namespace will be created
            vnet_id: The ID of the Virtual Network
            subnet_id: The ID of the Subnet to create private endpoint in
            capacity: The capacity of the namespace (1-16)
            queues: List of queue configurations
            topics: List of topic configurations
            tags: A dictionary of tags to assign to the namespace
            enable_private_endpoint: Whether to create a private endpoint
        """
        # Create the Service Bus namespace
        self.namespace = servicebus.Namespace(
            resource_name=name,
            resource_group_name=resource_group_name,
            location=location,
            namespace_name=name,
            sku=servicebus.SBSkuArgs(
                name="Premium",
                tier="Premium",
                capacity=capacity,
            ),
            tags=tags or {},
        )

        # Create queues
        self.queues = {}
        if queues:
            for queue_config in queues:
                queue_name = queue_config.get("name")

                queue = servicebus.Queue(
                    resource_name=f"{name}-{queue_name}",
                    resource_group_name=resource_group_name,
                    namespace_name=name,
                    queue_name=queue_name,
                    max_size_in_megabytes=queue_config.get("max_size_in_megabytes"),
                    lock_duration=queue_config.get("lock_duration"),
                    requires_duplicate_detection=queue_config.get(
                        "requires_duplicate_detection", False
                    ),
                    requires_session=queue_config.get("requires_session", False),
                    dead_lettering_on_message_expiration=queue_config.get(
                        "dead_lettering_on_message_expiration", False
                    ),
                    opts=pulumi.ResourceOptions(depends_on=[self.namespace]),
                )

                self.queues[queue_name] = queue

        # Create topics and subscriptions
        self.topics = {}
        self.subscriptions = {}
        if topics:
            for topic_config in topics:
                topic_name = topic_config.get("name")

                topic = servicebus.Topic(
                    resource_name=f"{name}-{topic_name}",
                    resource_group_name=resource_group_name,
                    namespace_name=name,
                    topic_name=topic_name,
                    max_size_in_megabytes=topic_config.get("max_size_in_megabytes"),
                    requires_duplicate_detection=topic_config.get(
                        "requires_duplicate_detection", False
                    ),
                    support_ordering=topic_config.get("support_ordering", False),
                    opts=pulumi.ResourceOptions(depends_on=[self.namespace]),
                )

                self.topics[topic_name] = topic

                # Create subscriptions for the topic
                subscriptions_config = topic_config.get("subscriptions", [])
                for sub_config in subscriptions_config:
                    sub_name = sub_config.get("name")

                    subscription = servicebus.Subscription(
                        resource_name=f"{name}-{topic_name}-{sub_name}",
                        resource_group_name=resource_group_name,
                        namespace_name=name,
                        topic_name=topic_name,
                        subscription_name=sub_name,
                        max_delivery_count=sub_config.get("max_delivery_count", 10),
                        dead_lettering_on_message_expiration=sub_config.get(
                            "dead_lettering_on_message_expiration", False
                        ),
                        opts=pulumi.ResourceOptions(depends_on=[topic]),
                    )

                    self.subscriptions[f"{topic_name}|{sub_name}"] = subscription

        # Create private endpoint if enabled
        if enable_private_endpoint and vnet_id and subnet_id:
            self.private_endpoint = network.PrivateEndpoint(
                resource_name=f"{name}-pe",
                resource_group_name=resource_group_name,
                location=location,
                private_endpoint_name=f"{name}-pe",
                subnet=network.SubnetArgs(id=subnet_id),
                private_link_service_connections=[
                    network.PrivateLinkServiceConnectionArgs(
                        name=f"{name}-pe-connection",
                        private_link_service_id=self.namespace.id,
                        group_ids=["namespace"],
                    ),
                ],
                tags=tags or {},
                opts=pulumi.ResourceOptions(depends_on=[self.namespace]),
            )

    @property
    def id(self) -> pulumi.Output[str]:
        """Get the ID of the namespace."""
        return self.namespace.id

    @property
    def name(self) -> pulumi.Output[str]:
        """Get the name of the namespace."""
        return self.namespace.name
