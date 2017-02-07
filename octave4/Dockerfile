FROM ubuntu:16.04
MAINTAINER Jeongkyu Shin "jshin@lablup.com"

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
RUN apt-get install -y liboctave-dev info
RUN apt-get install -y libzmq3-dev
WORKDIR /home/sorna
ADD *.deb /tmp/
RUN dpkg -i /tmp/*.deb

# Install script
ADD install_zeromq.m /home/sorna/install_zeromq.m
RUN octave-cli /home/sorna/install_zeromq.m

RUN mkdir /home/sorna/jsonlab
COPY jsonlab.tgz /home/sorna/jsonlab.tgz
RUN tar xvzf /home/sorna/jsonlab.tgz -C /home/sorna/

# Secure installation scripts
USER root
RUN apt-get install -yf libseccomp2
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/jail /home/sorna/intra-jail
RUN chmod 755 /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

RUN apt-get install -y ghostscript epstool transfig
RUN apt-get install -y pstoedit
RUN apt-get install -y gnuplot

ADD run.m /home/sorna/run.m
ADD run.sh /home/sorna/run.sh
RUN chown root:root /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh

ENV HOME /home/work
WORKDIR /home/work

VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
