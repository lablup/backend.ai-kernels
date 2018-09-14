FROM golang:1.11-alpine
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apk add --no-cache build-base libseccomp-dev git linux-headers && \
    mkdir -p /go/src/github.com/lablup/backend.ai-jail
WORKDIR /go/src/github.com/lablup/backend.ai-jail
RUN git clone https://github.com/lablup/backend.ai-jail . && \
    go get -u github.com/fatih/color && \
    go get -u github.com/mattn/go-isatty && \
    go get -u github.com/seccomp/libseccomp-golang && \
    go get -u github.com/gobwas/glob && \
    go get -u gopkg.in/yaml.v2
RUN make inside-container

# vim: ft=dockerfile
