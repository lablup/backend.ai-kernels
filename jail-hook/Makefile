
all:
	docker run --rm -it -v $(shell pwd):/root jail-hook-dev /bin/sh -c 'cd /root; make inner_all'

inner_all:
	gcc -Wall -shared -fPIC -o patch-libs.so patch-libs.c -ldl

manylinux:
	docker run --rm -it -v $(shell pwd):/root jail-hook-dev-ubuntu /bin/sh -c 'cd /root; make inner_manylinux'

inner_manylinux:
	gcc -Wall -shared -fPIC -o patch-libs-manylinux.so patch-libs.c -ldl
