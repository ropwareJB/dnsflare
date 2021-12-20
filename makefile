
BIN_PATH := $(shell cd src && stack path --dist-dir --allow-different-user)
bin: FORCE
	cd src && stack build
	make copy

watch: FORCE
	cd src && stack build --allow-different-user --file-watch --exec "make -C ./.. copy"

copy: FORCE
	cp src/${BIN_PATH}/build/dnsflare/dnsflare bin/;

clean:
	rm bin/*

.PHONY: clean

FORCE: ;
