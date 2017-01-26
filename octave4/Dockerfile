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
WORKDIR /home/work

# Install latest Octave
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get install -y build-essential
RUN apt-get install -y liboctave-dev
RUN apt-get install -y libzmq3-dev
WORKDIR /home/sorna
ADD *.deb /tmp/
RUN dpkg -i /tmp/*.deb

# Use PPA (when public PPA supports octave 4.2)
#RUN add-apt-repository -y ppa:octave/stable
#RUN apt-get update
#RUN apt-get install -y octave

# Install script
ADD install_zeromq.m /home/sorna/install_zeromq.m
RUN octave-cli /home/sorna/install_zeromq.m

RUN mkdir /home/sorna/jsonlab
COPY jsonlab.tgz /home/sorna/jsonlab.tgz
RUN tar xvzf /home/sorna/jsonlab.tgz -C /home/sorna/

# Secure installation scripts
USER root
RUN apt-get install -y libseccomp2
ADD run.m /home/sorna/run.m
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

ENV HOME /home/work
WORKDIR /home/work

VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
