FROM ubuntu:16.04
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

# Install latest node.js 4.x
ENV DEBIAN_FRONTEND noninteractive
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y curl build-essential
RUN apt-get install -y libreadline-dev libncursesw5-dev libssl-dev libsqlite3-dev libgdbm-dev libc6-dev libbz2-dev
RUN apt-get install -y wget
WORKDIR /home/sorna
RUN wget https://www.python.org/ftp/python/2.7.11/Python-2.7.11.tgz && tar xzvf Python-2.7.11.tgz
WORKDIR /home/sorna/Python-2.7.11
RUN ./configure && make altinstall
RUN ln -s /usr/local/bin/python2.7 /usr/bin/python
RUN curl -sL https://deb.nodesource.com/setup_4.x | bash -
RUN apt-get install -y nodejs

# Install packages
WORKDIR /home/sorna
RUN apt-get install -y libzmq3-dev git-core
ADD package.json /home/sorna/package.json
RUN cd /home/sorna; npm install; cd $HOME

# Secure installation scripts
USER root
ADD run.js /home/sorna/run.js
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
RUN apt-get install -y libseccomp2
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
