FROM lablup/kernel-base:python3.6-alpine

ENV MAKEFLAGS -j
ENV MAKEOPTS -j
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    apk update && apk add --no-cache build-base

WORKDIR /root

RUN pip install -U pip setuptools
RUN pip install --no-binary --no-cache-dir Cython
RUN pip install --no-binary --no-cache-dir numpy

RUN apk add --no-cache libuv-dev zeromq-dev snappy-dev geos-dev proj4-dev \
    jpeg-dev zlib-dev lapack-dev gcc freetype-dev gfortran musl-dev

RUN pip wheel --no-deps Cython
RUN pip wheel --no-deps numpy
RUN pip wheel --no-deps matplotlib
RUN pip wheel --no-deps pyproj
RUN pip wheel --no-deps Cartopy
RUN pip wheel --no-deps Pillow
RUN pip wheel --no-deps scipy
RUN pip wheel --no-deps pandas
RUN pip wheel --no-deps seaborn
RUN pip wheel --no-deps Pygments
RUN pip wheel --no-deps ipython
RUN pip wheel --no-deps PyWavelets
RUN pip wheel --no-deps networkx
RUN pip wheel --no-deps CVXcanon
RUN pip wheel --no-deps fastcache
RUN pip wheel --no-deps ecos
RUN pip wheel --no-deps cvxpy
RUN pip wheel --no-deps scikit-learn
RUN pip wheel --no-deps scikit-image

RUN pip wheel --no-deps pyzmq
RUN pip wheel --no-deps msgpack
RUN pip wheel --no-deps uvloop
RUN pip wheel --no-deps python-snappy

# List up built wheel packages
RUN ls -lh /root

# vim: ft=dockerfile
