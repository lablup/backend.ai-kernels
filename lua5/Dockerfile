FROM lablup/kernel-base-python3-minimal:latest

ENV LUA_VERSION 5.3
ENV LUA_PACKAGE lua${LUA_VERSION}

# Install Lua
RUN apk add --no-cache ${LUA_PACKAGE} ${LUA_PACKAGE}-libs unzip \
    && apk add --no-cache --virtual .build-deps build-base libc-dev curl ${LUA_PACKAGE}-dev \
    && cd /home/sorna \
    && curl -L http://luarocks.org/releases/luarocks-2.4.2.tar.gz -o luarocks-2.4.2.tar.gz \
    && tar zxpf luarocks-2.4.2.tar.gz \
    && cd /home/sorna/luarocks-2.4.2 \
    && ./configure; make bootstrap \
    && cd /home/sorna \
    && rm -rf luarocks-2.4.2 \
    && apk del .build-deps
RUN ln -s /usr/bin/${LUA_PACKAGE} /usr/bin/lua

# Install kernel-runner scripts package
# TODO: use PyPI's package if available
RUN apk add --no-cache git
RUN cd /home/sorna && \
    git clone https://github.com/lablup/backend.ai-kernel-runner.git
RUN cd /home/sorna/backend.ai-kernel-runner && pip install -e .[lua]
# RUN pip install backend.ai-kernel-runner[lua]
RUN apk add --no-cache ${LUA_PACKAGE}

LABEL io.sorna.features "query uid-match"

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "lua"]
