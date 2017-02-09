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

# Install Python
USER root
ENV SORNA_PYTHON_VERSION 3.6.0
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install -y --only-upgrade tzdata
RUN apt-get install -y build-essential git-core wget ca-certificates libseccomp2 \
		       libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev tk-dev
ADD install-python.sh /home/sorna/install-python.sh
ADD pyenv-run.sh /home/sorna/pyenv-run.sh
RUN chmod +x /home/sorna/*.sh
USER work
ENV PYENV_ROOT /home/sorna/.pyenv
ENV PATH /home/sorna/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN git clone https://github.com/yyuu/pyenv /home/sorna/.pyenv
RUN /home/sorna/install-python.sh

# Install Git extras
USER root
RUN apt-get install -y git-flow

# Install common Python packages
USER root
RUN apt-get install -y libjpeg-dev libpng-dev libzmq3-dev
USER work
# matplotlib may install older version of dateutil; we first install it.
RUN /home/sorna/pyenv-run.sh pip install python-dateutil
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist six
RUN /home/sorna/pyenv-run.sh pip install matplotlib

# Install specialization packages
USER root
RUN apt-get install -y libffi-dev cmake
RUN mkdir -p /tmp/libgit-build; \
    cd /tmp/libgit-build; \
    wget https://github.com/libgit2/libgit2/archive/v0.25.0.tar.gz; \
    tar xzf v0.25.0.tar.gz; \
    cd libgit2-0.25.0/; \
    /home/sorna/pyenv-run.sh cmake .; \
    /home/sorna/pyenv-run.sh make ; \
    /home/sorna/pyenv-run.sh make install; \
    rm -rf /tmp/libgit-build
USER work
RUN /home/sorna/pyenv-run.sh pip install pygit2
RUN /home/sorna/pyenv-run.sh pip install aiozmq uvloop

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
RUN /home/sorna/pyenv-run.sh pip install /tmp/sorna_media-0.3.0-py2.py3-none-any.whl

# Editors and other packages
USER root
RUN apt-get install -y nano vim
RUN apt-get install -y nodejs nodejs-legacy npm

# Secure installation scripts
USER root
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
ADD run.sh /home/sorna/run.sh
RUN chown root:root /home/sorna/*
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL io.sorna.timeout="10"
LABEL io.sorna.maxmem="128m"
LABEL io.sorna.mode="query+pty"
LABEL io.sorna.version="2"

ADD run.py /home/sorna/run.py

USER work
ENV PYTHONUNBUFFERED 1
CMD /home/sorna/run.sh
