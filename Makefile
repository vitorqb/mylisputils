.PHONY: build

build:
	rm -rf ./build && mkdir ./build
	cp ./mylisputils.el ./build/mylisputils.el
	$(eval version = $(shell git describe --tags --long))
	sed -i "s/____VERSION____/$(version)/g" ./build/mylisputils.el
	cd ./build && tar -vzcf "mylisputils-$(version).tar.gz" ./* && cd ..
