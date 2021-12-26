terraform {
  required_providers {
    vultr = {
      source = "vultr/vultr"
      version = "2.8.1"
    }
  }
}

# Configure the Vultr Provider
provider "vultr" {
  rate_limit = 700
  retry_limit = 3
}
