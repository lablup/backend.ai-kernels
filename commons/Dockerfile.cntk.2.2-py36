# Build target: lablup/common-cntk:2.2-py36
FROM ubuntu:18.04

# Install system package dependencies
# NOTE: running bazel requires JDK, not JRE!
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

WORKDIR \tmp

RUN curl -O https://cntk.ai/PythonWheel/CPU-Only/cntk-2.2-cp36-cp36m-linux_x86_64.whl

RUN ls -l /tmp/*.whl

# vim: ft=dockerfile sts=4 sw=4 et tw=0
