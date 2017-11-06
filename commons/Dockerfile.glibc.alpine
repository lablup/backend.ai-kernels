# Our build target: lablup/common-glibc-alpine
# ref: https://github.com/jeanblanchard/docker-alpine-glibc/blob/master/Dockerfile
# Modified to include latest beta dev package

FROM alpine:3.6
MAINTAINER Jean Blanchard <jean@blanchard.io>

ENV GLIBC_VERSION 2.25-r1

# Download and install glibc
# NOTE: "unreleased" should be changed to ${GLIBC_VERSION} when released.
RUN apk add --update curl && \
  curl -Lo /etc/apk/keys/sgerrand.rsa.pub https://raw.githubusercontent.com/sgerrand/alpine-pkg-glibc/master/sgerrand.rsa.pub && \
  curl -Lo glibc.apk "https://github.com/sgerrand/alpine-pkg-glibc/releases/download/unreleased/glibc-${GLIBC_VERSION}.apk" && \
  curl -Lo glibc-dev.apk "https://github.com/sgerrand/alpine-pkg-glibc/releases/download/unreleased/glibc-dev-${GLIBC_VERSION}.apk" && \
  curl -Lo glibc-bin.apk "https://github.com/sgerrand/alpine-pkg-glibc/releases/download/unreleased/glibc-bin-${GLIBC_VERSION}.apk" && \
  apk add glibc-bin.apk glibc-dev.apk glibc.apk && \
  /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib && \
  echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf && \
  apk del curl && \
  rm -rf glibc.apk glibc-bin.apk /var/cache/apk/*

# vim: ft=dockerfile et tw=0
