FROM ubuntu:16.04

ENV TERM=xterm-256color

RUN sed -i 's/archive/kr.archive/' /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y vim git-core
RUN apt-get install -y build-essential
RUN apt-get install -y golang
RUN apt-get install -y libseccomp2 libseccomp-dev

RUN mkdir -p /root/workspace/src
ENV GOPATH=/root/workspace
WORKDIR /root/workspace

RUN go get github.com/seccomp/libseccomp-golang
RUN go get github.com/fatih/color

CMD /bin/bash
