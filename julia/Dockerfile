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

# Install latest Julia
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y software-properties-common wget build-essential
RUN add-apt-repository -y ppa:staticfloat/juliareleases
RUN add-apt-repository -y ppa:staticfloat/julia-deps
RUN apt-get update
RUN apt-get install -y julia

# Install ZeroMQ
WORKDIR /home/sorna
RUN wget "https://github.com/zeromq/zeromq4-1/releases/download/v4.1.5/zeromq-4.1.5.tar.gz"
RUN tar xf zeromq-4.1.5.tar.gz
WORKDIR /home/sorna/zeromq-4.1.5
RUN ./configure
RUN make
RUN make install
RUN ldconfig
WORKDIR /home/sorna
RUN rm -rf zeromq*

# Install Julia packages
RUN mkdir -p /usr/local/share/julia/site/
ENV JULIA_PKGDIR "/usr/local/share/julia/site"
RUN chown work:work /usr/local/share/julia/site/
USER work
ADD package.jl /home/sorna/package.jl
RUN julia /home/sorna/package.jl

# Secure installation scripts
USER root
ADD run.jl /home/sorna/run.jl
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
