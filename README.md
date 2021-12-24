
## DNSFlare
A simple DNS service that relays queries to a webhook for notifications to assist detection of OOB or pseudo-blind vulnerabilities during penetration testing activities.

#### Note
To reduce notification noise, as DNS clients will often query for both IPv4 and IPv6, queries will be cached and only unique queries will receive the webhook notification. Currently the limit is hardcoded at 10. So you must send 10 additional unique requests before a given request for a particular domain will produce another webhook.

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

#### Webhook Notification
The webhook notification will take each domain from the DNS query and produce a JSON blob as follows to be `POST`ed to the webhook:

```
{ "text" : "`<domain>`",
  "username" :  "DNS Flare"
}
```
