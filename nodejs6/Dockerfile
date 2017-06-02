FROM lablup/kernel-base:latest

LABEL io.sorna.envs.corecount="NPROC"

# reference: https://github.com/nodejs/docker-node/blob/master/6.10/alpine/Dockerfile

ENV NPM_CONFIG_LOGLEVEL info
ENV YARN_VERSION 0.24.4

RUN apk add --no-cache \
        libgcc libstdc++ "nodejs=6.10.3-r0" \
    && apk add --no-cache --virtual .build-deps \
        curl \
        gnupg \
    && for key in \
      6A010C5166006599AA17F08146C2130DFD2497F5 \
    ; do \
      gpg --keyserver pgp.mit.edu --recv-keys "$key" || \
      gpg --keyserver keyserver.pgp.com --recv-keys "$key" || \
      gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
    done \
    && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz" \
    && curl -fSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc" \
    && gpg --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
    && mkdir -p /opt/yarn \
    && tar -xzf yarn-v$YARN_VERSION.tar.gz -C /opt/yarn --strip-components=1 \
    && ln -s /opt/yarn/bin/yarn /usr/local/bin/yarn \
    && ln -s /opt/yarn/bin/yarn /usr/local/bin/yarnpkg \
    && rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz

COPY package.json /home/sorna/package.json

RUN apk add --no-cache zeromq \
    && apk add --no-cache --virtual .build-deps \
        build-base python curl linux-headers zeromq-dev \
    && cd /home/sorna \
    && yarn global add node-gyp \
    && yarn install \
    && apk del .build-deps

COPY run.js /home/sorna/run.js

USER work
CMD ["/home/sorna/jail", "default", "/usr/bin/node", "/home/sorna/run.js"]

# vim: sts=4 sw=4 et
