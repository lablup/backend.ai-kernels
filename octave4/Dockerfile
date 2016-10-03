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

# Install latest Octave
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y software-properties-common build-essential
RUN apt-get install -y liboctave-dev
RUN add-apt-repository -y ppa:octave/stable
RUN apt-get install -y octave
ENV HOME /home/sorna
WORKDIR /home/sorna

# Install Git
RUN apt-get install -y git-core git-flow wget

# Install ZeroMQ
RUN wget "https://github.com/zeromq/zeromq4-1/releases/download/v4.1.5/zeromq-4.1.5.tar.gz"
RUN tar xf zeromq-4.1.5.tar.gz
WORKDIR /home/sorna/zeromq-4.1.5
RUN ./configure
RUN make
RUN make install
RUN ldconfig
WORKDIR /home/sorna
RUN rm -rf zeromq*

# Install zeromq-octave
RUN git clone https://github.com/esromneb/zeromq-octave.git
WORKDIR /home/sorna/zeromq-octave
RUN make
RUN cp zmq.mex /home/work/
WORKDIR /home/sorna
RUN rm -rf zeromq-octave

# Secure installation scripts
USER root
ADD run.m /home/sorna/run.m
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
RUN apt-get install -y libseccomp2
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
