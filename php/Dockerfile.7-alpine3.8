FROM alpine:3.8

# Install PHP 7.1
RUN apk add --no-cache --virtual .build-deps \
      build-base \
      pkgconf \
      autoconf \
      automake \
      libedit-dev \
      libxml2-dev \
      readline-dev \
      freetype-dev \
      libjpeg-turbo-dev \
      libpng-dev \
      libwebp-dev \
      pcre-dev \
      libxpm-dev && \
    apk add --no-cache php7 \
      php7-dev \
      php7-xml \
      php7-pcntl \
      php7-pear \
      php7-iconv \
      php7-zlib \
      php7-json \
      php7-gd \
      php7-sqlite3 && \
    # PECL patch (ref: http://stackoverflow.com/questions/40999752)
    sed -i "$ s|\-n||g" /usr/bin/pecl && \
    apk del .build-deps

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="php" \
      ai.backend.runtime-path="/usr/bin/php" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
