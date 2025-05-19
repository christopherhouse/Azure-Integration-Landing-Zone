# Reusable Private DNS Zone Module
resource "azurerm_private_dns_zone" "this" {
  name                = var.zone_name
  resource_group_name = var.resource_group_name
}

resource "azurerm_private_dns_zone_virtual_network_link" "this" {
  name                  = var.link_name
  resource_group_name   = var.resource_group_name
  private_dns_zone_name = azurerm_private_dns_zone.this.name
  virtual_network_id    = var.vnet_id
  registration_enabled  = false
}

resource "azurerm_private_dns_a_record" "this" {
  count               = var.create_a_record ? 1 : 0
  name                = var.a_record_name
  zone_name           = azurerm_private_dns_zone.this.name
  resource_group_name = var.resource_group_name
  ttl                 = 300
  records             = [var.a_record_ip]
}

output "zone_name" {
  value = azurerm_private_dns_zone.this.name
}
