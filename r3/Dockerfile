FROM ubuntu:16.04
MAINTAINER DevOps "devops@lablup.com"

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

# Set up the base environment.
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install -y --only-upgrade tzdata
RUN apt-get install -y build-essential git-core curl wget ca-certificates libseccomp2 libzmq3-dev
CMD /home/sorna/run.sh

# Install latest R
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | apt-key add -
RUN apt-get update
RUN apt-get install -y r-base libzmq3-dev
ADD install-packages.R /home/sorna/install-packages.R
RUN Rscript /home/sorna/install-packages.R

# Secure installation scripts
USER root
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
EXPOSE 2000 2001

LABEL io.sorna.timeout="10"
LABEL io.sorna.maxmem="128m"
LABEL io.sorna.maxcores="1"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,NPROC"
LABEL io.sorna.version="2"

ADD run.R /home/sorna/run.R
USER work
