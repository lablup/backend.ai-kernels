FROM lablup/kernel-base:latest

LABEL io.sorna.envs.corecount="NPROC"

# reference: https://github.com/nodejs/docker-node/blob/master/6.10/alpine/Dockerfile

ENV NPM_CONFIG_LOGLEVEL info
ENV YARN_VERSION 0.21.3

RUN apk add --no-cache \
        libgcc libstdc++ "nodejs=6.9.2-r1" \
    && apk add --no-cache --virtual .build-deps \
        curl \
        gnupg \
    && for key in \
      6A010C5166006599AA17F08146C2130DFD2497F5 \
    ; do \
      gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
    done \
    && curl -fSL -o yarn.js "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-legacy-$YARN_VERSION.js" \
    && curl -fSL -o yarn.js.asc "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-legacy-$YARN_VERSION.js.asc" \
    && gpg --batch --verify yarn.js.asc yarn.js \
    && rm yarn.js.asc \
    && mv yarn.js /usr/local/bin/yarn \
    && chmod +x /usr/local/bin/yarn \
    && apk del .build-deps

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
