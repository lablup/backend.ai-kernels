FROM lablup/kernel-base:latest

# Install Lua
RUN apk add --no-cache lua5.3 lua5.3-libs unzip zeromq \
    && apk add --no-cache --virtual .build-deps build-base libc-dev curl lua5.3-dev zeromq-dev \
    && cd /home/sorna \
    && curl -L http://luarocks.org/releases/luarocks-2.4.2.tar.gz -o luarocks-2.4.2.tar.gz \
    && tar zxpf luarocks-2.4.2.tar.gz \
    && cd /home/sorna/luarocks-2.4.2 \
    && ./configure; make bootstrap \
    && cd /home/sorna \
    && rm -rf luarocks-2.4.2 \
    && luarocks install lzmq \
    && apk del .build-deps

COPY run.lua /home/sorna/run.lua
USER work
CMD ["/home/sorna/jail", "default", "/usr/bin/lua5.3", "/home/sorna/run.lua"]
