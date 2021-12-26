FROM debian:11-slim AS builder
RUN ( \
	apt-get update && apt-get install -y libgmp10 libc6 curl && \
	curl -sSL https://get.haskellstack.org/ | sh && \
	mkdir /app && mkdir /app/bin \
)
COPY src /app/src
COPY makefile /app/
RUN (cd /app && make bin)

FROM debian:11-slim
RUN ( \
	apt-get update && apt-get install -y libgmp10 libc6 ca-certificates && \
	mkdir /app && touch /app/config \
)
COPY --from=builder /app/bin/dnsflare /app/dnsflare
WORKDIR /app
EXPOSE 53/tcp
EXPOSE 53/udp
CMD ["./dnsflare"]
