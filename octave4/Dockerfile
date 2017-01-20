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
RUN apt-get install -y software-properties-common build-essential libzmq3-dev
RUN apt-get install -y liboctave-dev
RUN apt-get install -y g++ make gawk gfortran gnuplot texi2html icoutils libxft-dev gperf flex libbison-dev libqhull-dev libglpk-dev libcurl4-gnutls-dev libfltk1.3-dev librsvg2-dev libqrupdate-dev libgl2ps-dev libarpack2-dev libqscintilla2-dev libreadline-dev libncurses5-dev libhdf5-dev llvm-dev libqt4-dev default-jdk texinfo libfftw3-dev libgraphicsmagick++-dev libjasper-dev libfreeimage-dev transfig epstool librsvg2-bin libosmesa6-dev
WORKDIR /home/sorna
RUN apt-get install -y wget
RUN wget ftp://ftp.gnu.org/gnu/octave/octave-4.2.0.tar.lz 
RUN apt-get install -y lzip
RUN lzip -d octave-4.2.0.tar.lz
RUN tar xf octave-4.2.0.tar
WORKDIR /home/sorna/octave-4.2.0
#RUN ./configure --enable-jit
RUN ./configure
RUN make -j7
RUN make install
#RUN make check
#RUN add-apt-repository -y ppa:octave/stable
#RUN apt-get update
#RUN apt-get install -y octave
ADD install_zeromq.m /home/sorna/install_zeromq.m
RUN octave-cli /home/sorna/install_zeromq.m

RUN mkdir /home/sorna/jsonlab
#COPY jsonlab/*.m /home/sorna/jsonlab/
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
