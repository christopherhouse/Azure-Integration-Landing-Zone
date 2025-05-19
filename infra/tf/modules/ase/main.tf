# Deploys an internal App Service Environment (ASE) with a private DNS zone

resource "azurerm_app_service_environment_v3" "ase" {
  name                = var.ase_name
  resource_group_name = var.resource_group_name
  location            = var.location
  subnet_id           = var.subnet_id
  internal_load_balancing_mode = "Web, Publishing"
  zone_redundant      = false
  # Add more settings as needed
}

resource "azurerm_private_dns_zone" "ase" {
  name                = "privatelink.azurewebsites.net"
  resource_group_name = var.resource_group_name
}

resource "azurerm_private_dns_a_record" "ase" {
  name                = var.ase_name
  zone_name           = azurerm_private_dns_zone.ase.name
  resource_group_name = var.resource_group_name
  ttl                 = 300
  records             = [azurerm_app_service_environment_v3.ase.internal_inbound_ip_address]
}

resource "azurerm_private_dns_a_record" "ase_scm" {
  name                = "${var.ase_name}.scm"
  zone_name           = azurerm_private_dns_zone.ase.name
  resource_group_name = var.resource_group_name
  ttl                 = 300
  records             = [azurerm_app_service_environment_v3.ase.internal_inbound_ip_address]
}
