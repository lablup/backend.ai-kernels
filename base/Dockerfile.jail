FROM golang:1.11-stretch
MAINTAINER Mario Cho "m.cho@lablup.com"

# Debian stretch ships libseccomp 2.1 but golang binding requires 2.2+
RUN echo "deb http://ftp.debian.org/debian stretch-backports main" > /etc/apt/sources.list.d/backports.list && \
    apt update -y && \
    apt -t stretch-backports install -y libseccomp-dev && \
    rm -r /var/lib/apt/lists /var/cache/apt/archives && \
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
