// Deploys an internal App Service Environment (ASE) with a private DNS zone

resource "azurerm_app_service_environment_v3" "app_service_environment" {
  name                = var.app_service_environment_name
  resource_group_name = var.resource_group_name
  subnet_id           = var.subnet_id
  internal_load_balancing_mode = "Web, Publishing"
  zone_redundant      = false
  tags                = var.tags
  # Add more settings as needed
}

module "private_dns_zone" {
  source              = "../private_dns_zone"
  zone_name           = "${var.app_service_environment_name}.appserviceenvironment.net"
  resource_group_name = var.resource_group_name
  link_name           = "${var.app_service_environment_name}-vnet-link"
  vnet_id             = var.vnet_id
  tags                = var.tags
}

module "dns_a_record_wildcard" {
  source              = "../private_dns_a_record"
  zone_name           = module.private_dns_zone.zone_name
  resource_group_name = var.resource_group_name
  name                = "*"
  records             = azurerm_app_service_environment_v3.app_service_environment.internal_inbound_ip_addresses
  ttl                 = 300
}

module "dns_a_record_wildcard_scm" {
  source              = "../private_dns_a_record"
  zone_name           = module.private_dns_zone.zone_name
  resource_group_name = var.resource_group_name
  name                = "*.scm"
  records             = azurerm_app_service_environment_v3.app_service_environment.internal_inbound_ip_addresses
  ttl                 = 300
}
