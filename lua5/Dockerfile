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
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install -y --only-upgrade tzdata
RUN apt-get install -y wget unzip ca-certificates build-essential
RUN apt-get install -y libzmq3-dev

# Install Lua
USER root
RUN apt-get install -y lua5.2 liblua5.2-dev

# Install Lua packages using luarocks pkg manager
WORKDIR /home/sorna
RUN wget http://luarocks.org/releases/luarocks-2.4.2.tar.gz
RUN tar zxpf luarocks-2.4.2.tar.gz
WORKDIR /home/sorna/luarocks-2.4.2
RUN ./configure; make bootstrap
WORKDIR /home/sorna
RUN rm -rf luarocks-2.4.2
RUN luarocks install lzmq

# Secure installation scripts
USER root
ADD run.lua /home/sorna/run.lua
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

ENV HOME /home/work
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2001

LABEL io.sorna.timeout="10"
LABEL io.sorna.maxmem="128m"
LABEL io.sorna.maxcores="1"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="NPROC"
LABEL io.sorna.version="2"

USER work
CMD /home/sorna/run.sh
