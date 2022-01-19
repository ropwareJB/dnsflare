#!/bin/bash

function deploy() {
	IP=$1
	ssh -oStrictHostKeyChecking=accept-new root@$IP -t -- "{
		apt-get update
		apt-get install -y libgmp10 libc6 screen
		ufw allow 53/udp
		ufw allow 53/tcp
	}"


	# Stop any existing service while we update
	ssh -f root@$IP "sh -c '{
		systemctl stop dnsflare
	}'"

	scp config root@$IP:~/
	scp dnsflare.service root@$IP:/etc/systemd/system/dnsflare.service
	scp bin/dnsflare-debian root@$IP:~/dnsflare
	ssh -f root@$IP "sh -c '{
		ln -s /root/dnsflare /usr/bin/dnsflare
		systemctl daemon-reload
		systemctl start dnsflare
		systemctl enable dnsflare
	}'"
}

deploy $(terraform output -state=terraform/terraform.tfstate -raw ns1_ip_addr)
deploy $(terraform output -state=terraform/terraform.tfstate -raw ns2_ip_addr)
