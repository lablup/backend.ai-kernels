FROM ubuntu:14.04
MAINTAINER Jonghyun Park "jpark@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for node.js and REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna

ENV HOME /home/work
WORKDIR /home/work

# Install node.js 4.2.3
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y nodejs
RUN apt-get install -y npm
RUN ln -s `which nodejs` /usr/bin/node

# Install packages
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:chris-lea/zeromq
RUN add-apt-repository -y ppa:chris-lea/libpgm
RUN apt-get update
RUN apt-get install -y libzmq3-dev
ADD package.json /home/sorna/package.json
RUN cd /home/sorna; npm install; cd $HOME

# Secure installation scripts
USER root
ADD run.js /home/sorna/run.js
ADD run.sh /home/sorna/run.sh
RUN chown root:root /home/sorna/*.sh
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh

VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
