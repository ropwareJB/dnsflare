resource "vultr_ssh_key" "my_ssh_key" {
  name = "my-ssh-key"
  ssh_key = "${file("~/.ssh/id_rsa.pub")}"
}

resource "vultr_instance" "ns1" {
  plan = "vc2-1c-1gb"
  region = "syd"
  os_id = "477"
  label = "dnsflare-ns1"
  tag = "dnsflare"
  hostname = "ns1"
  enable_ipv6 = false
  backups = "disabled"
  ddos_protection = false
  activation_email = false
  ssh_key_ids = [vultr_ssh_key.my_ssh_key.id]
}

resource "vultr_instance" "ns2" {
  plan = "vc2-1c-1gb"
  region = "syd"
  os_id = "477"
  label = "dnsflare-ns2"
  tag = "dnsflare"
  hostname = "ns2"
  enable_ipv6 = false
  backups = "disabled"
  ddos_protection = false
  activation_email = false
  ssh_key_ids = [vultr_ssh_key.my_ssh_key.id]
}

output "ns1_ip_addr" {
	value = vultr_instance.ns1.main_ip
}
output "ns2_ip_addr" {
	value = vultr_instance.ns2.main_ip
}
