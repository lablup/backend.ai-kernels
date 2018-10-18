FROM lablup/kernel-base:python3.6 as python-binary
# ------------------------
FROM lablup/kernel-base:ubuntu
ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1

RUN apt update -y && \
    apt install -y \
        ca-certificates \
        wget \
        libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g \
	libmpdec2 \
        libssl1.0.0 \
	libssl-dev \
        libncursesw5 libtinfo5 libreadline6 \
        libproj-dev libgeos-dev \
        mime-support \
        libzmq3-dev libuv1

# Copy the whole Python from the docker library image
COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
# Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

# As we mostly have "manylinux" glibc-compatible binary packaes,
# we don't have to rebuild these!
RUN pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2"

# vim: ft=dockerfile
