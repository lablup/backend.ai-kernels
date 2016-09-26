FROM ubuntu:16.04
MAINTAINER Joongi Kim "joongi@lablup.com"

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

# Install Git
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git-core git-flow wget

# Install Python
USER root
ENV SORNA_PYTHON_VERSION 3.5.2
RUN apt-get build-dep -y python3
RUN apt-get install -y libreadline-dev libsqlite3-dev libssl-dev libbz2-dev
ADD install-python.sh /home/sorna/install-python.sh
ADD pyenv-run.sh /home/sorna/pyenv-run.sh
RUN chmod +x /home/sorna/*.sh
USER work
ENV PYENV_ROOT /home/sorna/.pyenv
ENV PATH /home/sorna/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN git clone https://github.com/yyuu/pyenv /home/sorna/.pyenv
RUN /home/sorna/install-python.sh

# Install common Python packages
USER root
RUN apt-get install -y pkg-config
RUN apt-get install -y libjpeg-dev libpng-dev libzmq3-dev tk-dev
USER work
# matplotlib may install older version of dateutil; we first install it.
RUN /home/sorna/pyenv-run.sh pip install python-dateutil
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist six
RUN /home/sorna/pyenv-run.sh pip install matplotlib

# Matplotlib configuration
USER root
ENV MPLCONFIGDIR /home/sorna/.matplotlib
RUN mkdir /home/sorna/.matplotlib
RUN chown -R work:work /home/sorna/.matplotlib
USER work
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py
RUN MPLBACKEND=Agg /home/sorna/pyenv-run.sh python /tmp/matplotlib-fontcache.py
RUN rm /tmp/matplotlib-fontcache.py

# Install Sorna Media support
USER root
ADD *.whl /tmp
ADD matplotlibrc /home/sorna/.matplotlib/
USER work
RUN /home/sorna/pyenv-run.sh pip install /tmp/sorna_media-0.2.0-py2.py3-none-any.whl

# Secure installation scripts
USER root
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
RUN apt-get install -y libseccomp2
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
ADD run.sh /home/sorna/run.sh
ADD run.py /home/sorna/run.py
RUN chown root:root /home/sorna/*
RUN chmod 600 /home/sorna/*
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail

VOLUME ["/home/work"]
EXPOSE 2001

LABEL com.lablup.sorna.timeout="10"
LABEL com.lablup.sorna.maxmem="128m"

USER work
ENV PYTHONUNBUFFERED 1
CMD /home/sorna/run.sh
