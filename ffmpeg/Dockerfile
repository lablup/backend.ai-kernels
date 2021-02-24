FROM nvidia/cuda:11.1-devel-ubuntu18.04

ENV DEBIAN_FRONTEND noninteractive
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES video,compute,utility

ARG NASM_VER="2.15.05"
ARG YASM_VER="1.3.0"
ARG LAME_VER="3.100"
ARG FFMPEG_VER="4.3.2"

# Install required Packages

RUN set -xe && \
    apt-get update && apt-get install -y aptitude && aptitude update && \
    aptitude install -y \
        wget build-essential automake autoconf git libtool libvorbis-dev \
        libass-dev libfreetype6-dev libsdl2-dev libva-dev libvdpau-dev \
        libxcb1-dev libxcb-shm0-dev libxcb-xfixes0-dev \
        mercurial libnuma-dev texinfo zlib1g-dev \
        cmake qtbase5-dev && \
    mkdir -p /usr/local/ffmpeg_sources

# Instaling NASM
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    wget -O /usr/local/ffmpeg_sources/nasm.tar.bz2 \
    	 https://www.nasm.us/pub/nasm/releasebuilds/${NASM_VER}/nasm-${NASM_VER}.tar.bz2 && \
    mkdir -p /usr/local/ffmpeg_sources/nasm && \
    tar jxvf /usr/local/ffmpeg_sources/nasm.tar.bz2 \
        -C /usr/local/ffmpeg_sources/nasm --strip-components 1 && \
    cd /usr/local/ffmpeg_sources/nasm && \
    ./autogen.sh && \
    ./configure --prefix=/usr/local && \
    make -j$(nproc) && \
    make install

# Installing YASM
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    wget -O /usr/local/ffmpeg_sources/yasm.tar.gz \
    	 https://www.tortall.net/projects/yasm/releases/yasm-${YASM_VER}.tar.gz && \
    mkdir -p /usr/local/ffmpeg_sources/yasm && \
    tar xzvf /usr/local/ffmpeg_sources/yasm.tar.gz \
        -C /usr/local/ffmpeg_sources/yasm --strip-components 1 && \
    cd /usr/local/ffmpeg_sources/yasm && \
    ./configure --prefix=/usr/local && \
    make -j$(nproc) && \
    make install

# libx264
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    git -C /usr/local/ffmpeg_sources/x264 pull 2> /dev/null || \
#   git clone --depth 1 https://git.videolan.org/git/x264 && \
    git clone --depth 1 https://code.videolan.org/videolan/x264 && \
    cd /usr/local/ffmpeg_sources/x264 && \
    ./configure --enable-static --enable-pic && \
    make && \
    make install

# libx265
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    git clone https://github.com/videolan/x265 \
        /usr/local/ffmpeg_sources/x265 && \
    cd /usr/local/ffmpeg_sources/x265/build/linux && \
    cmake -G "Unix Makefiles" -DENABLE_SHARED=off ../../source && \
    make -j$(nproc) && \
    make install

# libvpx
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    git -C libvpx pull 2> /dev/null || \
    	git clone --depth 1 https://chromium.googlesource.com/webm/libvpx.git && \
    cd /usr/local/ffmpeg_sources/libvpx && \
    ./configure --disable-examples --disable-unit-tests --enable-vp9-highbitdepth --as=yasm && \
    make -j$(nproc) && \
    make install

# libfdk-aac
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    git -C fdk-aac pull 2> /dev/null || \
    	git clone --depth 1 https://github.com/mstorsjo/fdk-aac && \
    cd fdk-aac && \
    autoreconf -fiv && \
    ./configure --disable-shared && \
    make -j$(nproc) && \
    make install

# libmp3lame
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    wget -O /usr/local/ffmpeg_sources/lame.tar.gz \
         https://downloads.sourceforge.net/project/lame/lame/${LAME_VER}/lame-${LAME_VER}.tar.gz && \
    mkdir -p /usr/local/ffmpeg_sources/lame && \
    tar xzvf /usr/local/ffmpeg_sources/lame.tar.gz \
         -C /usr/local/ffmpeg_sources/lame --strip-components 1 && \
    cd /usr/local/ffmpeg_sources/lame && \
    ./configure --disable-shared --enable-nasm && \
    make -j$(nproc) && \
    make install

# install libopus 
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    git -C opus pull 2> /dev/null || \
    	git clone --depth 1 https://github.com/xiph/opus && \
    cd opus && \
    ./autogen.sh && \
    ./configure --disable-shared && \
    make -j$(nproc) && \
    make install

# install nvidias codec API
WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    git -C nv-codec-headers pull 2> /dev/null || \
        git clone https://github.com/FFmpeg/nv-codec-headers && \
    cd nv-codec-headers && \
    make -j$(nproc) && \
    make install

WORKDIR /usr/local/ffmpeg_sources
RUN set -xe && \
    wget -O /usr/local/ffmpeg_sources/ffmpeg.tar.bz2 \
         https://ffmpeg.org/releases/ffmpeg-${FFMPEG_VER}.tar.bz2 && \
    mkdir -p /usr/local/ffmpeg_sources/ffmpeg && \
    tar jxvf /usr/local/ffmpeg_sources/ffmpeg.tar.bz2 \
         -C /usr/local/ffmpeg_sources/ffmpeg --strip-components 1 && \
    cd /usr/local/ffmpeg_sources/ffmpeg && \
    ./configure \
        --pkg-config-flags="--static" \
	--extra-libs="-lpthread -lm" \
	--enable-gpl \
	--enable-libass \
	--enable-libnpp \
	--enable-libfdk-aac \
	--enable-libfreetype \
	--enable-libmp3lame \
	--enable-libopus \
	--enable-libvorbis \
	--enable-libvpx \
	--enable-libx264 \
	--enable-libx265 \
	--enable-static \
	--enable-cuda \
	--enable-cuvid \
	--enable-nvenc \
	--enable-libnpp \
	--extra-cflags=-I/usr/local/cuda/include \
	--extra-ldflags=-L/usr/local/cuda/lib64 \
	--enable-nonfree && \
    make -j$(nproc) && \
    make install

WORKDIR /
RUN set -xe && \
    apt-get clean && \
    rm -rf /usr/local/ffmpeg_sources

COPY ./service-defs /etc/backend.ai/service-defs
COPY ./policy.yml /etc/backend.ai/jail/policy.yml

#ENTRYPOINT ["/usr/local/bin/ffmpeg"]
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/ffmpeg" 

WORKDIR /home/work