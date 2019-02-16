# Build target: lablup/common-cntk:2.6-py36
FROM ubuntu:18.04

# Install system package dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    	ca-certificates \
        curl gcc g++ make cmake git \
	libssh-dev \
	proj-bin libproj-dev \
	libgeos-dev libgeos++-dev \
	mime-support \
        libcurl3-dev \
        libfreetype6-dev \
        libhdf5-serial-dev \
        libpng-dev \
        libzmq3-dev \
        libcups2 \
        pkg-config \
        rsync \
        imagemagick \
        graphviz \
        rsync \
        sed \
        zip zip unzip \
        zlib1g-dev \
	&& \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

#install Python 3
RUN curl https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tar.xz -o /opt/python.tar.xz && \
      cd /opt && tar xvf python.tar.xz && \
      cd /opt/*/ && ./configure && \
      make && make install && \
      ln -s -f /usr/local/bin/python3.6 /usr/bin/python
WORKDIR \tmp

RUN curl -fsSL -O https://bootstrap.pypa.io/get-pip.py && \
    python get-pip.py && \
    rm get-pip.py

RUN curl -O https://cntk.ai/PythonWheel/CPU-Only/cntk-2.6-cp36-cp36m-linux_x86_64.whl 

RUN ls -l /tmp/*.whl

# vim: ft=dockerfile sts=4 sw=4 et tw=0
