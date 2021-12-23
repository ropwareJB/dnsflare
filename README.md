
## DNSFlare
A simple DNS service that relays queries to a webhook for notifications to assist detection of OOB or pseudo-blind vulnerabilities during penetration testing activities.

#### Usage
Create a file `config` which contains a single line representing a HTTPS endpoint to POST received queries to.
```
> cat config
https://hooks.slack.com/services/xxxxxxxxxxx/xxxxxxxxxxx/xxxxxxxxxxxxxxxxxxxxxxxx
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

#### Compiling
```
make bin
```
Binary will be in `./bin/dnsflare`.
