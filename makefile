
BIN_PATH := $(shell cd src && stack path --dist-dir --allow-different-user)
PWD := $(shell pwd)
bin: FORCE
	cd src && stack build
	make copy

watch: FORCE
	cd src && stack build --allow-different-user --file-watch --exec "make -C ./.. copy"

copy: FORCE
	cp src/${BIN_PATH}/build/dnsflare/dnsflare bin/;

docker: FORCE
	docker build -f docker/Dockerfile -t dnsflare .

docker-server: FORCE
	sudo docker run -p 53:53/tcp -p 53:53/udp -v ${PWD}/bin/config:/app/config dnsflare

debian: FORCE
	docker create -ti --name dummy dnsflare bash
	docker cp dummy:/app/dnsflare ./bin/dnsflare-debian
	docker rm -f dummy

clean:
	rm bin/dnsflare
	cd src && stack clean --full

.PHONY: clean

FORCE: ;
