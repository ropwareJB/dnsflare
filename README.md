
## DNSFlare
A simple DNS service that relays queries to a webhook for notifications to assist detection of OOB or pseudo-blind vulnerabilities during penetration testing activities.

#### Note
To reduce notification noise, as DNS clients will often query for both IPv4 and IPv6, queries will be cached and only unique queries will receive the webhook notification. The limit defaults to 10. So you must send 10 additional unique requests before a given request for a particular domain will produce another webhook. See the Help section to configure this setting.

#### Usage
Create a file `config` which contains a single line representing a HTTPS endpoint to POST received queries to.
```
> cat config
https://hooks.slack.com/services/xxxxxxxxxxx/xxxxxxxxxxx/xxxxxxxxxxxxxxxxxxxxxxxx
> sudo ./dnsflare
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
  -? --help              Display help message
  -V --version           Print version information
```

By default, the cache length is 10. You can change it with the `-c` CLI arg.
