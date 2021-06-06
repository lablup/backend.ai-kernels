FROM nvidia/cuda:10.1-devel-ubuntu18.04

# TensorFlow version is tightly coupled to CUDA and cuDNN so it should be selected carefully
ARG CUDA=10.1
ARG LIBNVINFER=6.0.1-1
ARG LIBNVINFER_MAJOR_VERSION=6
ENV CUDNN_VERSION=7.6.5.32
ENV NCCL=2.7.8
ENV NCCL_VERSION=2.7.8-1+cuda10.1
ENV MLNX_OFED_VERSION=5.1-2.3.7.1
ENV MKL_VERSION=2020.3
ENV MKL_BUILD=279
ENV NUMPY_VERSION 1.19.4
ENV SCIPY_VERSION 1.5.4
ENV OPENMPI 4.0.5
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES video,compute,utility

ARG NASM_VER="2.15.05"
ARG YASM_VER="1.3.0"
ARG LAME_VER="3.100"
ARG FFMPEG_VER="4.3.2"

# Python 3.8 is supported by Ubuntu Bionic out of the box
ARG python=3.6
ENV PYTHON_VERSION=${python}

ENV DEBIAN_FRONTEND=noninteractive \
    MPLBACKEND=Svg \
    CPLUS_INCLUDE_PATH=/usr/include/gdal \
    C_INCLUDE_PATH=/usr/include/gdal \
    PYTHONUNBUFFERED=1 \
    LIBRARY_PATH=/usr/local/cuda/lib64/stubs \
    _CUDA_COMPAT_PATH="/usr/local/cuda/compat" \
    LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:/usr/local/cuda/compat/lib:/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/include/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu:/opt/intel/compilers_and_libraries_2020.3.279/linux/mkl/lib/intel64" \
    PATH="/usr/local/mpi/bin:/usr/local/ucx:/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/bin/cmake/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/tensorrt/bin:/usr/local/src/lightgbm/LightGBM" \
    LANG=C.UTF-8

# Set default shell to /bin/bash
SHELL ["/bin/bash", "-cu"]

RUN rm -fr /var/lib/apt/lists/* && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends software-properties-common wget && \
    add-apt-repository -y "deb http://security.ubuntu.com/ubuntu xenial-security main" && \
    add-apt-repository -y ppa:deadsnakes/ppa && \
    add-apt-repository -y ppa:bashtop-monitor/bashtop && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
	autoconf \
	autoconf-archive \
	automake \
	build-essential \
	ca-certificates \
	dkms \
	fonts-nanum \
	fonts-nanum-coding \
	fonts-nanum-extra \
	freeglut3 \
	freeglut3-dev \
	gfortran \
	gir1.2-gtk-3.0 \
	gphoto2 \
	graphviz \
	htop bashtop \
	iputils-ping \
	libasound2-dev \
	libass-dev \
	libatlas-base-dev \
	libavcodec-dev \
	libavformat-dev \
	libavresample-dev \
	libcairo2-dev \
	libcanberra-gtk-module \
	libcudnn7-dev=$CUDNN_VERSION-1+cuda10.1 \
	libcudnn7=$CUDNN_VERSION-1+cuda10.1 \
	libcurl3-dev \
	libdc1394-22 \
	libdc1394-22-dev \
	libeigen3-dev \
	libfaac-dev \
	libfreetype6-dev \
	libgdal-dev \
	libgeos-dev libgeos++-dev \
	libgflags-dev \
	libgirepository1.0-dev \
	libgoogle-glog-dev \
	libgphoto2-dev \
	libgraphicsmagick1-dev \
	libgstreamer-plugins-base1.0-dev \
	libgstreamer1.0-dev \
	libgtk-3-dev \
	libgtk2.0-dev \
	libhdf5-dev \	
	libhdf5-serial-dev \
	libjasper-dev \
	libjpeg-dev \
	liblapack-dev \
	liblapacke-dev \
	libleptonica-dev \
	libmp3lame-dev \
	libmpdec2 \
	libnuma-dev \
	libogre-1.9-dev \
	libopenblas-dev \ 
	libopencore-amrnb-dev \
	libopencore-amrwb-dev \
	libpng-dev \
	libportmidi-dev \
	libpq-dev \
	libproj-dev \
	libprotobuf-dev \
	libsdl-dev \
	libsdl-image1.2-dev \
	libsdl-mixer1.2-dev \
	libsdl-ttf2.0-dev \
	libsdl2-dev \
	libsm6 \
	libsmpeg-dev \
	libssl-dev \
	libswscale-dev \
	libtbb-dev \
	libtbb2 \
	libtesseract-dev \
	libtheora-dev \
	libtiff-dev \
	libtiff5-dev \
	libtool \
	libv4l-dev \
	libva-dev \
	libvdpau-dev \
	libvorbis-dev \
	libx264-dev \
	libxcb-shm0-dev \
	libxcb-xfixes0-dev \
	libxcb1-dev \
	libxext6 \
	libxi-dev \
	libxine2-dev \
	libxml2-dev \
	libxmu-dev \
	libxrender-dev \
	libxslt1-dev \
	libxvidcore-dev \
	libzmq3-dev \
	mercurial \
	mime-support \
	ninja-build \
	pbzip2 \
	pdsh curl net-tools \
	pkg-config \
	proj-bin \
	protobuf-compiler \
	python${PYTHON_VERSION} \
	python${PYTHON_VERSION}-dev \
	python3-gdal \
	qt5-default \
	qtbase5-dev \
	rsync \
	software-properties-common \
	tesseract-ocr \
	tesseract-ocr-jpn \
	texinfo \
	v4l-utils \
	vim \
	wget zip unzip \
	x264 \
	xvfb \
	xz-utils \
	yasm \
  	openssh-client openssh-server \
        build-essential \
        gcc-6 g++-6 make \
        git subversion \
        zlib1g-dev \
	&& \
    ln -s /usr/include/libv4l1-videodev.h /usr/include/linux/videodev.h && \
    find /usr/local/cuda-${CUDA}/lib64/ -type f -name 'lib*_static.a' -not -name 'libcudart_static.a' -delete

WORKDIR /tmp
COPY nccl_2.7.8-1+cuda10.1_x86_64.txz .
RUN tar --no-same-owner --keep-old-files --lzma -xvf nccl_${NCCL}-1+cuda10.1_x86_64.txz -C /usr/local/cuda/lib64/ --strip-components=2 --wildcards '*/lib/libnccl.so.*' && \
    tar --no-same-owner --keep-old-files --lzma -xvf  nccl_${NCCL}-1+cuda10.1_x86_64.txz -C /usr/lib/pkgconfig/ --strip-components=3 --wildcards '*/lib/pkgconfig/*' && \
    rm nccl_${NCCL}-1+cuda10.1_x86_64.txz && \
    ldconfig 

RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 && \
    echo "/usr/local/cuda/lib64/stubs" > /etc/ld.so.conf.d/z-cuda-stubs.conf && \
    ldconfig

# Install CUDA-10.1 + cuDNN 7.6.5
RUN ln -s /usr/local/cuda-10.1 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.6.5 /usr/local/cuda/lib64/libcudnn.so && \
    rm -rf /var/lib/apt/lists/* && \
    ldconfig

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        libnvinfer${LIBNVINFER_MAJOR_VERSION}=${LIBNVINFER}+cuda${CUDA} \
        libnvinfer-dev=${LIBNVINFER}+cuda${CUDA} \
        libnvinfer-plugin-dev=${LIBNVINFER}+cuda${CUDA} \
        libnvinfer-plugin${LIBNVINFER_MAJOR_VERSION}=${LIBNVINFER}+cuda${CUDA} && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# OFED
WORKDIR /tmp
RUN wget -q -O - https://www.mellanox.com/downloads/ofed/MLNX_OFED-5.1-2.3.7.1/MLNX_OFED_LINUX-${MLNX_OFED_VERSION}-ubuntu18.04-x86_64.tgz | tar xzf - && \
    cd /tmp/MLNX_OFED_LINUX-${MLNX_OFED_VERSION}-ubuntu18.04-x86_64 && \
    ./mlnxofedinstall --user-space-only --without-fw-update --all -q && \
    cd /tmp && \
    rm -rf /tmp/MLNX_OFED_LINUX-${MLNX_OFED_VERSION}-ubuntu18.04-x86_64*


# nvidia-peer
WORKDIR /tmp
RUN git clone https://github.com/Mellanox/nv_peer_memory.git /tmp/nv_peer_memory && \
    cd /tmp/nv_peer_memory && \
    ./build_module.sh && \
    cd /tmp && \
    tar xzf /tmp/nvidia-peer-memory_1.1.orig.tar.gz && \
    cd /tmp/nvidia-peer-memory-1.1 && \
    dpkg-buildpackage -us -uc && \
    dpkg -i /tmp/nvidia-peer-memory_1.1-0_all.deb

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        libboost-all-dev \
	libboost-dev \
	libboost-filesystem-dev \
	libboost-python-dev \
	libboost-system-dev \
	libboost-thread-dev

RUN if [[ "${PYTHON_VERSION}" == "3.6" ]]; then \
        apt-get install -y python${PYTHON_VERSION}-distutils python-apt ; \
    fi


# Add OpenCL ICD files for LightGBM
RUN mkdir -p /etc/OpenCL/vendors && \
    echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd



RUN wget https://github.com/Kitware/CMake/releases/download/v3.20.3/cmake-3.20.3-Linux-x86_64.sh \
         -q -O /tmp/cmake-install.sh && \
    chmod u+x /tmp/cmake-install.sh && \
    mkdir /usr/bin/cmake && \
    /tmp/cmake-install.sh --skip-license --prefix=/usr/bin/cmake && \
    rm /tmp/cmake-install.sh


RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

WORKDIR /tmp
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools pip

RUN python3 -m pip install --no-cache-dir \
    	    Cython==0.29.21 \
	    tornado==6.1 \
	    nvidia-pyindex==1.0.5 \
	    matplotlib==3.3.3 

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

RUN ln -s /usr/include/x86_64-linux-gnu/cudnn_v7.h /usr/local/cuda/include/cudnn.h 

# OpenCV
WORKDIR /tmp
ENV OPENCV_VERSION="4.5.2"
COPY libnvcuvid.so.455.45.01 /usr/lib/x86_64-linux-gnu/libnvcuvid.so.1
RUN wget https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip && \
    wget -O opencv-contrib.zip https://github.com/opencv/opencv_contrib/archive/${OPENCV_VERSION}.zip && \
    unzip ${OPENCV_VERSION}.zip && \
    unzip opencv-contrib.zip && \
    mkdir opencv-${OPENCV_VERSION}/cmake_binary 
RUN cd /tmp/opencv-${OPENCV_VERSION}/cmake_binary && \
    cmake \
      -D BUILD_DOCS=OFF \
      -D BUILD_NEW_PYTHON_SUPPORT=ON \
      -D BUILD_PERF_TESTS=OFF \
      -D BUILD_PERF_TESTS=OFF \
      -D BUILD_TESTS=OFF \
      -D BUILD_TIFF=ON \
      -D BUILD_opencv_apps=ON \
      -D BUILD_opencv_calib3d=ON \
      -D BUILD_opencv_core=ON \
      -D BUILD_opencv_cudaarithm=ON \
      -D BUILD_opencv_cudabgsegm=ON \
      -D BUILD_opencv_cudacodec=ON \
      -D BUILD_opencv_cudafeatures2d=ON \
      -D BUILD_opencv_cudafilters=ON \
      -D BUILD_opencv_cudaimgproc=ON \
      -D BUILD_opencv_cudalegacy=ON \
      -D BUILD_opencv_cudaobjdetect=ON \
      -D BUILD_opencv_cudaoptflow=ON \
      -D BUILD_opencv_cudastereo=ON \
      -D BUILD_opencv_cudawarping=ON \
      -D BUILD_opencv_cudev=ON \
      -D BUILD_opencv_features2d=ON \
      -D BUILD_opencv_flann=ON \
      -D BUILD_opencv_highgui=ON \
      -D BUILD_opencv_imgcodecs=ON \
      -D BUILD_opencv_imgproc=ON \
      -D BUILD_opencv_java=OFF \
      -D BUILD_opencv_ml=ON \
      -D BUILD_opencv_objdetect=ON \
      -D BUILD_opencv_photo=ON \
      -D BUILD_opencv_python2=OFF \
      -D BUILD_opencv_python3=ON \
      -D BUILD_opencv_stitching=ON \
      -D BUILD_opencv_superres=ON \
      -D BUILD_opencv_ts=ON \
      -D BUILD_opencv_video=ON \
      -D BUILD_opencv_videoio=ON \
      -D BUILD_opencv_videostab=ON \
      -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=$(python3 -c "import sys; print(sys.prefix)") \
#      -D CUDA_ARCH_BIN=7.5 \
      -D CUDA_ARCH_PTX=7.5 \
      -D CUDA_BIN_PATH=/usr/local/cuda \
      -D CUDA_CUDA_LIBRARY=/usr/lib/x86_64-linux-gnu/stubs/libcuda.so \
      -D CUDA_FAST_MATH=ON \
      -D CUDA_HOST_COMPILER=/usr/bin/gcc-6 \
      -D CUDA_NVCC_FLAGS="-D_FORCE_INLINES --expt-relaxed-constexpr" \
      -D CUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda-10.1 \
      -D CUDNN_INCLUDE_DIR=/usr/local/cuda/include \
      -D CUDNN_LIBRARY=/usr/lib/x86_64-linux-gnu/libcudnn.so.7.6.5 \
      -D ENABLE_AVX=ON \
      -D ENABLE_FAST_MATH=1 \
      -D HAVE_opencv_python3=ON \
      -D INSTALL_C_EXAMPLES=OFF \
      -D INSTALL_PYTHON_EXAMPLES=OFF \
      -D OPENCV_DNN_CUDA=ON \
      -D OPENCV_ENABLE_NONFREE=ON \
      -D OPENCV_EXTRA_MODULES_PATH="../../opencv_contrib-4.5.2/modules" \
      -D OPENCV_GENERATE_PKGCONFIG=ON \
      -D OPENCV_SKIP_PYTHON_LOADER=ON \
      -D PYTHON_DEFAULT_EXECUTABLE=python3 \
      -D PYTHON_EXECUTABLE=$(which python3) \
      -D PYTHON_INCLUDE_DIR2=$(python3 -c "from os.path import dirname; from distutils.sysconfig import get_config_h_filename; print(dirname(get_config_h_filename()))") \
      -D PYTHON_INCLUDE_DIR=$(python3 -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())") \
      -D PYTHON_LIBRARY=$(python3 -c "from distutils.sysconfig import get_config_var;from os.path import dirname,join ; print(join(dirname(get_config_var('LIBPC')),get_config_var('LDLIBRARY')))") \
      -D PYTHON_PACKAGES_PATH=$(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())") \
      -D WITH_1394=OFF \
      -D WITH_CUBLAS=ON \
      -D WITH_CUDA=ON \
      -D WITH_CUDNN=ON \
      -D WITH_CUFFT=ON \
      -D WITH_EIGEN=ON \
      -D WITH_FFMPEG=ON \
      -D WITH_GDAL=ON \
      -D WITH_GIGEAPI=ON \
      -D WITH_GPHOTO2=ON \
      -D WITH_GSTREAMER=ON \
      -D WITH_GTK=ON \
      -D WITH_IMGCODEC=ON \
      -D WITH_INTELPERC=OFF \
      -D WITH_IPP=ON \
      -D WITH_IPP_A=OFF \
      -D WITH_JASPER=ON \
      -D WITH_JPEG=ON \
      -D WITH_LAPACK=OFF \
      -D WITH_LIBV4L=ON \
      -D WITH_NVCUVID=ON \
      -D WITH_OPENCL=ON \
      -D WITH_OPENCLAMDBLAS=OFF \
      -D WITH_OPENCLAMDFFT=OFF \
      -D WITH_OPENCL_SVM=OFF \
      -D WITH_OPENEXR=ON \
      -D WITH_OPENGL=ON \
      -D WITH_OPENMP=OFF \
      -D WITH_OPENNI=OFF \
      -D WITH_PNG=ON \
      -D WITH_PROTOBUF=ON \
      -D WITH_PTHREADS_PF=OFF \
      -D WITH_PVAPI=ON \
      -D WITH_QT=OFF \
      -D WITH_TBB=ON \
      -D WITH_TIFF=ON \
      -D WITH_UNICAP=OFF \
      -D WITH_V4L=ON \
      -D WITH_VTK=ON \
      -D WITH_WEBP=ON \
      -D WITH_XIMEA=OFF \
      -D WITH_XINE=OFF \
      -D WITH_ZLIB=ON \
      -G Ninja \
      -D CMAKE_INSTALL_PREFIX=/usr/local \
      .. 2>&1 | tee cmake_messages.txt 
RUN cd /tmp/opencv-4.5.2/cmake_binary && ninja
#RUN make -j$(nproc) && make install 
RUN rm /usr/lib/x86_64-linux-gnu/libnvcuvid.so.1

RUN set -xe && \
    apt update -y && \
    apt install sudo && \
    git clone https://github.com/CMU-Perceptual-Computing-Lab/openpose && \
    cd /usr/local/openpose && \
    bash ./scripts/ubuntu/install_deps.sh && \
    mkdir -p /usr/local/openpose/build && \
    cd /usr/local/openpose/build && \
    cmake .. && \
    make -j$(nproc) && \
    make install


# Stream transcoding
COPY ./scripts/bootstrap.sh /opt/container/bootstrap.sh
RUN  chmod +x /opt/container/bootstrap.sh

# Backend.AI specifics
COPY ./service-defs /etc/backend.ai/service-defs
COPY ./policy.yml /etc/backend.ai/jail/policy.yml

# Clean up
WORKDIR /usr/local
RUN apt autoclean && \
    rm -rf /usr/local/ffmpeg_sources && \
    ln -s /usr/local/openpose/build/examples/openpose/openpose.bin /usr/local/bin/openpose.bin && \
    sed -i 's/source \/usr\/local\/nvm\/nvm.sh//' /etc/bash.bashrc && \
    rm -rf /var/lib/apt/lists/* && \	
    rm -rf /root/.cache && \
    rm -rf /tmp/*

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

