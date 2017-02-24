FROM ubuntu:16.04
MAINTAINER DevOps "devops@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for Python and REPL script
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

# Install common Python dependencies
RUN apt-get install -y libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev tk-dev
RUN apt-get install -y pkg-config libjpeg-dev libpng-dev
RUN apt-get install -y libjpeg-dev libpng-dev
RUN apt-get install -y libfreetype6-dev libblas-dev liblapack-dev libatlas-dev gfortran
CMD /home/sorna/run.sh

# Install Python installer (pyenv)
USER work
ENV PYENV_ROOT /home/sorna/.pyenv
ENV PATH /home/sorna/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN git clone https://github.com/yyuu/pyenv /home/sorna/.pyenv

# Install Python
ENV PYTHONUNBUFFERED 1
ENV SORNA_PYTHON_VERSION 3.6.0
ADD install-python.sh /home/sorna/install-python.sh
ADD pyenv-run.sh      /home/sorna/pyenv-run.sh
RUN /home/sorna/install-python.sh

# Install common Python packages
RUN /home/sorna/pyenv-run.sh pip install pyzmq aiozmq namedlist simplejson uvloop

# Install Haskell Platform and required packages
USER root
RUN apt-get install -y haskell-platform pkg-config
RUN cabal update

# Secure installation scripts
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

VOLUME ["/home/work"]
EXPOSE 2001

ADD run.py /home/sorna/run.py
USER work
