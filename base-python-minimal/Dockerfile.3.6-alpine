FROM lablup/kernel-base:python3.6-alpine as python-binary
FROM lablup/kernel-base-python-wheels:3.6-alpine as wheel-builds
# ------------------------
FROM lablup/kernel-base:alpine
ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1

# Consult this on https://pkgs.alpinelinux.org/package/edge/main/x86_64/python3
# NOTE: it should depend only on libressl but for somehow reason it actually requires openssl.
RUN apk add --no-cache ca-certificates \
        wget \
        expat gdbm libbz2 libffi openssl ncurses-libs readline sqlite-libs xz-libs zlib

# Install libressl2.7-libssl. edge/main repo should not be added before expat
# installation. That will break pip (incompatible version of libexpat.so.1?).
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && \
    apk update && apk add --no-cache libressl2.7-libssl

# Copy the whole Python from the docker library image
COPY --from=python-binary /python.tar.gz /
COPY --from=wheel-builds /root/pyzmq*.whl /tmp/
COPY --from=wheel-builds /root/msgpack*.whl /tmp/
COPY --from=wheel-builds /root/uvloop*.whl /tmp/
COPY --from=wheel-builds /root/python_snappy*.whl /tmp/
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz

# Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

# Lablup extension: install Backend.ai's common Python packages
RUN apk add --no-cache --virtual .sorna-deps zeromq libuv \
    && pip install --no-cache-dir /tmp/*.whl \
    && pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" \
    && rm /tmp/*.whl

# vim: ft=dockerfile
