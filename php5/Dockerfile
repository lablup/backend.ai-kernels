FROM ubuntu:14.04
MAINTAINER Joongi Kim "joongi@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for Python and REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna

ENV HOME /home/work
WORKDIR /home/work

# Set up package manager.
ENV DEBIAN_FRONTEND noninteractive
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list

# Install PHP 7.0
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git-core wget php5-cli php5-gd php5-dev php-pear
RUN apt-get install -y pkg-config libzmq3-dev
RUN echo '' | pecl install zmq-beta
RUN echo 'extension=zmq.so' > /etc/php5/cli/conf.d/30-zmq.ini

# Secure installation scripts
USER root
ADD run.php /home/sorna/run.php
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
RUN apt-get install -y libseccomp2
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail

VOLUME ["/home/work"]
EXPOSE 2001

USER work
CMD /home/sorna/run.sh
