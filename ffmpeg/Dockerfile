FROM nvidia/cuda:11.1-devel-ubuntu18.04

ENV DEBIAN_FRONTEND noninteractive
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES video,compute,utility

ARG NASM_VER="2.15.05"
ARG YASM_VER="1.3.0"
ARG LAME_VER="3.100"
ARG FFMPEG_VER="4.3.2"
ARG python=3.8
ENV PYTHON_VERSION=${python}

ENV DEBIAN_FRONTEND=noninteractive \
    MPLBACKEND=Svg \
    PYTHONUNBUFFERED=1 \
    LIBRARY_PATH=/usr/local/cuda/lib64/stubs \
    _CUDA_COMPAT_PATH="/usr/local/cuda/compat" \
    LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:/usr/local/cuda/compat/lib:/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64" \
    PATH="/usr/local/mpi/bin:/usr/local/ucx:/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/bin/cmake/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/tensorrt/bin:" \
    LANG=C.UTF-8

# Set default shell to /bin/bash
SHELL ["/bin/bash", "-cu"]

# Install required Packages

RUN set -xe && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends software-properties-common wget && \
    add-apt-repository -y ppa:deadsnakes/ppa && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
	ca-certificates \
	curl \
	dkms \
	fonts-nanum \
	fonts-nanum-coding \
	fonts-nanum-extra \
	g++ \
	gfortran \
	htop \
	iputils-ping \
	libasound2-dev \
	libavcodec-dev \
	libboost-dev \
	libboost-filesystem-dev \
	libboost-system-dev \
	libcairo2-dev libgirepository1.0-dev pkg-config gir1.2-gtk-3.0 \
	libgdal-dev python3-gdal \
	libnuma-dev \
	libopenblas-dev liblapack-dev \
	libsm6 \
	libswscale-dev \
	libv4l-dev \
	libxext6 \
	libxine2-dev \
	libxml2-dev \
	libxrender-dev \
	libxslt1-dev \
	make \
	mime-support \
	net-tools \
	openssh-server \
	pdsh \
	proj-bin libproj-dev libgeos-dev libgeos++-dev graphviz \
	subversion \
	unzip \
	xvfb \
	xz-utils \
	yasm \
	zip \
  	openssh-client \
        build-essential \
        gcc \
        git \
        libavformat-dev \
        libcurl3-dev \
        libfreetype6-dev \
        libgeos-dev libgeos++-dev \
        libhdf5-serial-dev \
        libjpeg-dev \
        libmpdec2 \
        libpng-dev \
        libsdl-dev libsdl-image1.2-dev libsdl-mixer1.2-dev libsdl-ttf2.0-dev \
        libsmpeg-dev libportmidi-dev libavformat-dev libswscale-dev \
        libssl-dev \
        libtiff-dev \
        libzmq3-dev \
        pkg-config \
        python${PYTHON_VERSION} \
        python${PYTHON_VERSION}-dev \
        vim \
        wget \
        zlib1g-dev \
        wget build-essential automake autoconf git libtool libvorbis-dev \
        libass-dev libfreetype6-dev libsdl2-dev libva-dev libvdpau-dev \
        libxcb1-dev libxcb-shm0-dev libxcb-xfixes0-dev \
        mercurial libnuma-dev texinfo zlib1g-dev \
        qtbase5-dev && \
    mkdir -p /usr/local/ffmpeg_sources && \
    ln -s /usr/include/libv4l1-videodev.h /usr/include/linux/videodev.h && \
    mkdir -p /etc/OpenCL/vendors && \
    echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icdn && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

WORKDIR /tmp
RUN curl -fsSL https://developer.download.nvidia.com/compute/redist/nccl/v2.8/nccl_2.8.4-1+cuda11.1_x86_64.txz -O && \
    tar --no-same-owner --keep-old-files --lzma -xvf nccl_2.8.4-1+cuda11.1_x86_64.txz -C /usr/local/cuda/lib64/ --strip-components=2 --wildcards '*/lib/libnccl.so.*' && \
    tar --no-same-owner --keep-old-files --lzma -xvf  nccl_2.8.4-1+cuda11.1_x86_64.txz -C /usr/lib/pkgconfig/ --strip-components=3 --wildcards '*/lib/pkgconfig/*' && \
    rm nccl_2.8.4-1+cuda11.1_x86_64.txz && \
    ldconfig 

RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 && \
    echo "/usr/local/cuda/lib64/stubs" > /etc/ld.so.conf.d/z-cuda-stubs.conf && \
    ldconfig

RUN wget https://github.com/Kitware/CMake/releases/download/v3.20.0/cmake-3.20.0-Linux-x86_64.sh \
         -q -O /tmp/cmake-install.sh && \
    chmod u+x /tmp/cmake-install.sh && \
    mkdir /usr/bin/cmake && \
    /tmp/cmake-install.sh --skip-license --prefix=/usr/bin/cmake && \
    rm /tmp/cmake-install.sh

RUN rm /usr/bin/python3 && \
    ln -s /usr/bin/python3.8 /usr/bin/python3 && \
    update-alternatives --install /usr/bin/python python /usr/bin/python3 1 && \
    update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.8 2


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

# OpenCV

WORKDIR /tmp
ENV OPENCV_VERSION="4.5.2"
RUN wget https://github.com/opencv/opencv/archive/refs/tags/${OPENCV_VERSION}.zip && \
    wget -O opencv-contrib.zip https://github.com/opencv/opencv_contrib/archive/${OPENCV_VERSION}.zip && \
    unzip ${OPENCV_VERSION}.zip && \
    unzip opencv-contrib.zip && \
    mkdir opencv-${OPENCV_VERSION}/cmake_binary && \
    cd opencv-${OPENCV_VERSION}/cmake_binary && \
    cmake \
      -DCMAKE_BUILD_TYPE=RELEASE \
      -D BUILD_TIFF=ON \
      -D BUILD_opencv_java=OFF \
      -D WITH_CUDA=ON \
      -D CUDA_NVCC_FLAGS=--expt-relaxed-constexpr \
      -D CUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda-11.1 \
      -D ENABLE_FAST_MATH=1 \
      -D CUDA_FAST_MATH=1 \
      -D WITH_CUBLAS=1 \
      -D WITH_OPENGL=ON \
      -D WITH_OPENCL=ON \
      -D WITH_IPP=ON \
      -D WITH_TBB=ON \
      -D WITH_EIGEN=ON \
      -D WITH_V4L=ON \
      -D WITH_FFMPEG=ON \
      -D WITH_GSTREAMER=ON \
      -D BUILD_TESTS=OFF \
      -D BUILD_PERF_TESTS=OFF \
      -D OPENCV_EXTRA_MODULES_PATH="../../opencv_contrib-4.5.2/modules" \
      -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=$(python3 -c "import sys; print(sys.prefix)") \
      -D PYTHON_EXECUTABLE=$(which python3) \
      -D PYTHON_INCLUDE_DIR=$(python3 -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())") \
      -D PYTHON_PACKAGES_PATH=$(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())") \
      .. 2>&1 | tee cmake_messages.txt && \
    make -j$(nproc) && \
    make install 

# clean up
WORKDIR /
RUN set -xe && \
    apt-get clean && \
    rm -rf /usr/local/ffmpeg_sources

# Stream transcoding
COPY ./scripts/bootstrap.sh /opt/container/bootstrap.sh
RUN  chmod +x /opt/container/bootstrap.sh

# Backend.AI specifics
COPY ./service-defs /etc/backend.ai/service-defs
COPY ./policy.yml /etc/backend.ai/jail/policy.yml

#ENTRYPOINT ["/usr/local/bin/ffmpeg"]
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.resource.min.cuda.device=0 \
      ai.backend.resource.min.cuda.shares=0 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/ffmpeg" \
      ai.backend.service-ports=""

WORKDIR /home/work
