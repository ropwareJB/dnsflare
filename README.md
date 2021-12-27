
## DNSFlare
A simple DNS service that relays queries to a webhook for notifications to assist detection of OOB or pseudo-blind vulnerabilities during penetration testing activities.

#### Note
To reduce notification noise, as DNS clients will often query for both IPv4 and IPv6, queries will be cached and only unique queries will receive the webhook notification. The limit defaults to 10. So you must send 10 additional unique requests before a given request for a particular domain will produce another webhook. See the Help section to configure this setting.

#### Usage
Create a file `config` which contains a single line representing a HTTPS endpoint to POST received queries to.
```
> cat config
https://hooks.slack.com/services/xxxxxxxxxxx/xxxxxxxxxxx/xxxxxxxxxxxxxxxxxxxxxxxx
> sudo ./dnsflare --monitor-domain=example.com
```

Run the service on at least two internet-routable hosts. Then, configure your DNS records to point to your two new nameservers.
```
> dig @1.1.1.1 ns e.example.com

...
;; AUTHORITY SECTION:
example.com.		21600	IN	NS	ns1.example.com.
example.com.		21600	IN	NS	ns2.example.com.
...
```

#### Docker
Build the docker container with the make command, then you can run the docker container forwarding your host UDP and TCP port 53 to the container. Share a webhook config file with a mount:
```
> make docker
> sudo docker run -p 53:53/tcp -p 53:53/udp -v $(pwd)/config:/app/config dnsflare
```

#### Compiling
Requires Haskell Stack.
```
make bin
```
Binary will be in `./bin/dnsflare`.

#### Webhook Notification
The webhook notification will take each domain from the DNS query and produce a JSON blob as follows to be `POST`ed to the webhook:

```
{ "text" : "`<domain>`",
  "username" :  "DNS Flare"
}
```

#### Help
```
> ./dnsflare --help

v0.1.0, 2021 ROPWARE, Joshua Brown

dnsflare [OPTIONS]
  DNS Flare

Common flags:
  -c --cache-length=INT   
  -m --monitor-domain=ITEM
  -? --help                 Display help message
  -V --version              Print version information
```

By default, the cache length is 10. You can change it with the `-c` CLI arg.

#### Deploying with Terraform
Add your webhook URL to a config file in the root directory of the repo:
```
> ls
bin  config  deploy.sh  docker  LICENSE  makefile  README.md  src  terraform
```

Grab your Vultr API key and export it as an environment variable, then run the terraform plan and the deployment script:
```
> export VULTR_API_KEY=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
> cd terraform
> terraform apply
> cd ..
> ./deploy.sh
```

Then set your DNS nameservers to the IPs returned by terraform output:
```
> terraform output -state=terraform/terraform.tfstate
ns1_ip_addr = "111.111.111.11"
ns2_ip_addr = "22.22.22.222"
```
