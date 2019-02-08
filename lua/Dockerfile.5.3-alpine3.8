FROM alpine:3.8

# Install image-specific add-ons
ENV LUA_VERSION 5.3
ENV LUA_PACKAGE lua${LUA_VERSION}

# Install Lua
RUN apk add --no-cache ${LUA_PACKAGE} ${LUA_PACKAGE}-libs openssl zip unzip \
    && apk add --no-cache --virtual .build-deps build-base libc-dev curl ${LUA_PACKAGE}-dev \
    && cd /tmp \
    && curl -L http://luarocks.org/releases/luarocks-3.0.4.tar.gz -o luarocks-3.0.4.tar.gz \
    && tar zxpf luarocks-3.0.4.tar.gz \
    && cd /tmp/luarocks-3.0.4 \
    && ./configure; make bootstrap \
    && cd /tmp \
    && rm -rf luarocks-3.0.4 \
    && apk del .build-deps
RUN rm -f /usr/bin/lua && ln -s /usr/bin/${LUA_PACKAGE} /usr/bin/lua

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="64m" \
      ai.backend.runtime-type="lua" \
      ai.backend.runtime-path="/usr/bin/lua" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
