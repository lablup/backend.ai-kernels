FROM alpine:3.8

ENV SCHEME_VERSION 9.2

RUN apk --no-cache --virtual build-dependencies add build-base m4 \
    && rm -f /var/cache/apk/* \
    && wget http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/${SCHEME_VERSION}/mit-scheme-${SCHEME_VERSION}-x86-64.tar.gz \
    && tar zxvf mit-scheme-${SCHEME_VERSION}-x86-64.tar.gz \
    && rm mit-scheme-${SCHEME_VERSION}-x86-64.tar.gz \
    && cd mit-scheme-${SCHEME_VERSION}/src \
    && ./configure \
    && make compile-microcode \
    && make install \
    && cd /tmp \
    && rm -rf cd mit-scheme-${SCHEME_VERSION} \
    && apk del build-dependencies

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="scheme" \
      ai.backend.runtime-path="/usr/bin/scm" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
