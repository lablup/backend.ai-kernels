FROM alpine:3.5

RUN apk add --no-cache build-base vim git gcc go libseccomp-dev linux-headers

RUN mkdir -p /root/workspace/src
ENV GOPATH=/root/workspace
WORKDIR /root/workspace/src/sorna-repl

RUN go get github.com/seccomp/libseccomp-golang
RUN go get github.com/fatih/color

ENV TERM=xterm-256color

CMD ["/bin/ash"]
