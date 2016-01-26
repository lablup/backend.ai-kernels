FROM ubuntu:14.04
MAINTAINER Joongi Kim "joongi@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna

ENV HOME /home/work

# Install latest R
ENV DEBIAN_FRONTEND noninteractive
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y curl build-essential
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list
ENV HOME /home/sorna
WORKDIR /home/sorna
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | apt-key add -
RUN apt-get update && apt-get install -y r-base

# Install packages
RUN apt-get install -y libzmq3-dev
ADD install-packages.R /home/sorna/install-packages.R
RUN Rscript /home/sorna/install-packages.R

# Secure installation scripts
USER root
ADD run.R /home/sorna/run.R
ADD run.sh /home/sorna/run.sh
RUN chown root:root /home/sorna/*.sh
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh

ENV HOME /home/work
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
