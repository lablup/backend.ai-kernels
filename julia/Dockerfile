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
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install -y --only-upgrade tzdata
RUN apt-get install -y software-properties-common wget ca-certificates build-essential libseccomp2
RUN add-apt-repository -y ppa:staticfloat/juliareleases
RUN add-apt-repository -y ppa:staticfloat/julia-deps
RUN apt-get update
RUN apt-get install -y julia

# Install ZeroMQ and Python 3.5 (for PyPlot)
RUN apt-get install -y libzmq3-dev python3.5 python3.5-dev python3-pip python3-tk
RUN pip3 install -U pip setuptools wheel
RUN pip3 install numpy matplotlib

# Configure Python's matplotlib and pre-heat font-cache.
USER root
RUN mkdir /home/sorna/.matplotlib
ENV MPLCONFIGDIR "/home/sorna/.matplotlib"
RUN chown -R work:work /home/sorna/.matplotlib
USER work
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py
RUN python3 /tmp/matplotlib-fontcache.py
RUN rm /tmp/matplotlib-fontcache.py
ADD matplotlibrc /home/sorna/.matplotlib/

# Install extra Julia dependencies (detected only at runtime)
USER root
RUN apt-get install -y libgmp-dev libmpfr-dev \
                       gettext libcairo2 libpango1.0 libpangocairo-1.0 \
                       libglib2.0-0

# Install Julia packages
USER root
RUN mkdir -p /usr/local/share/julia/site/
ENV JULIA_PKGDIR "/usr/local/share/julia/site"
RUN chown work:work /usr/local/share/julia/site/
USER work
ADD package.jl /home/sorna/package.jl
RUN julia /home/sorna/package.jl
ADD precompile.jl /home/sorna/precompile.jl
RUN julia /home/sorna/precompile.jl

# Secure installation scripts
USER root
ADD run.jl /home/sorna/run.jl
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

LABEL io.sorna.timeout="180"
LABEL io.sorna.maxmem="512m"
LABEL io.sorna.maxcores="4"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,JULIA_CPU_CORES"
LABEL io.sorna.version="2"

ENV HOME /home/work
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh

# vim: ts=8 sts=4 sw=4 et
