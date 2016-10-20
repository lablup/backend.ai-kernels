FROM ubuntu:16.04
MAINTAINER Jonghyun Park "jpark@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna

ENV DEBIAN_FRONTEND noninteractive
ENV HOME /home/work

# Install ZeroMQ
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y wget zip
RUN apt-get install -y libzmq3-dev

# Install Lua
USER root
RUN apt-get install -y lua5.2 liblua5.2-dev
RUN lua

# Install Lua packages
WORKDIR /home/sorna
RUN wget http://luarocks.org/releases/luarocks-2.4.1.tar.gz  # lua package manager
RUN tar zxpf luarocks-2.4.1.tar.gz
WORKDIR /home/sorna/luarocks-2.4.1
RUN ./configure; make bootstrap
WORKDIR /home/sorna
RUN rm -rf luarocks-2.4.1
RUN luarocks install lzmq  # zmq package
RUN luarocks install dkjson  # json package

# Secure installation scripts
USER root
RUN apt-get install -y libseccomp2
ADD run.lua /home/sorna/run.lua
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail

ENV HOME /home/work
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
