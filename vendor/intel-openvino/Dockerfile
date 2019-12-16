FROM ubuntu:18.04

ARG INSTALL_DIR=/opt/intel/openvino 

RUN apt-get update && apt-get -y upgrade && apt-get autoremove -y

#Install needed dependences
RUN apt-get install -y --no-install-recommends \
        build-essential \
        cpio \
        curl \
        git \
        lsb-release \
        pciutils \
	python3 python3-pip python3-dev \
        python3-setuptools 

RUN apt-get update -y && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y "deb http://security.ubuntu.com/ubuntu xenial-security main" && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
	openssh-server \
	libopenmpi-dev \
	openmpi-bin \
	openmpi-common \
	openmpi-doc \
	binutils \
	wget \
	unzip \
	vim \
        libpq-dev \
	libjasper-dev \
        libtiff-dev \
        libjpeg-dev \
        libpng-dev \
	libavcodec-dev \
        libavformat-dev \
	libswscale-dev \
	libxine2-dev \
	libv4l-dev \
	libboost-dev \
	libboost-system-dev \
	libboost-filesystem-dev \
	fonts-nanum \
	fonts-nanum-coding \
	fonts-nanum-extra

RUN ln -s /usr/include/libv4l1-videodev.h /usr/include/linux/videodev.h && \
   apt-get install -y \
	libgstreamer1.0-dev \
	libgstreamer-plugins-base1.0-dev \
	libgtk-3-dev \
        libtbb-dev \
	libatlas-base-dev \
	libdc1394-22-dev \
	libxvidcore-dev \
	libfaac-dev \
	libmp3lame-dev \
	libtheora-dev \
	libvorbis-dev \
	libxvidcore-dev \
	libopencore-amrnb-dev libopencore-amrwb-dev \
	libavresample-dev \
	x264 \
	libx264-dev \
	v4l-utils \	
	libprotobuf-dev protobuf-compiler \
	libgoogle-glog-dev libgflags-dev \
	libgphoto2-dev \
	libeigen3-dev \
	libhdf5-dev \	
    && \	
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

RUN wget https://github.com/Kitware/CMake/releases/download/v3.15.3/cmake-3.15.3-Linux-x86_64.sh \
         -q -O /tmp/cmake-install.sh && \
    chmod u+x /tmp/cmake-install.sh && \
    mkdir /usr/bin/cmake && \
    /tmp/cmake-install.sh --skip-license --prefix=/usr/bin/cmake && \
    rm /tmp/cmake-install.sh

ENV PATH="/usr/bin/cmake/bin:${PATH}"

WORKDIR /tmp
ENV OPENCV_VERSION="4.1.0"
RUN wget https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip && \
    wget -O opencv-contrib.zip https://github.com/opencv/opencv_contrib/archive/${OPENCV_VERSION}.zip && \
    unzip ${OPENCV_VERSION}.zip && \
    unzip opencv-contrib.zip && \
    mkdir opencv-${OPENCV_VERSION}/cmake_binary && \
    cd opencv-${OPENCV_VERSION}/cmake_binary && \
    cmake \
      -DCMAKE_BUILD_TYPE=RELEASE \
      -D BUILD_TIFF=ON \
      -D BUILD_opencv_java=OFF \
      -D WITH_CUDA=OFF \
      -D WITH_V4L=ON \
      -D BUILD_TESTS=OFF \
      -D BUILD_PERF_TESTS=OFF \
      -D OPENCV_EXTRA_MODULES_PATH="../../opencv_contrib-4.1.0/modules" \
      -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=$(python3 -c "import sys; print(sys.prefix)") \
      -D PYTHON_EXECUTABLE=$(which python3) \
      -D PYTHON_INCLUDE_DIR=$(python3 -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())") \
      -D PYTHON_PACKAGES_PATH=$(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())") \
      .. 2>&1 | tee cmake_messages.txt && \
    make -j${nproc} && \
    make install && \
    cd /tmp && \
    rm -fr opencv* 

WORKDIR /tmp
COPY ./requirements.txt /tmp
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools pip 
RUN python3 -m pip install --no-cache-dir -r requirements.txt

WORKDIR /tmp
# installing OpenVINO itself
RUN curl -O http://registrationcenter-download.intel.com/akdlm/irc_nas/16057/l_openvino_toolkit_p_2019.3.376.tgz && \
    tar -xvzf l_openvino_toolkit_p_2019.3.376.tgz && \
    cd l_openvino_toolkit_p_2019.3.376 && \
    bash ./install_openvino_dependencies.sh && \
    sed -i 's/decline/accept/g' silent.cfg && \
    bash ./install.sh -s silent.cfg && \
    bash $INSTALL_DIR/install_dependencies/install_cv_sdk_dependencies.sh && \
    mkdir $INSTALL_DIR/deployment_tools/inference_engine/samples/build && \
    cd $INSTALL_DIR/deployment_tools/inference_engine/samples/build && \
    bash -c "source $INSTALL_DIR/bin/setupvars.sh && cmake .. && make -j{nproc}" && \
    rm /tmp/l_openvino_toolkit_p_2019.3.376.tgz

# Model Optimizer
RUN cd $INSTALL_DIR/deployment_tools/model_optimizer/install_prerequisites && \
    bash ./install_prerequisites.sh

# clean up 
RUN apt autoremove -y && \
    rm -rf /tmp/* /var/lib/apt/lists/*

RUN /bin/bash -c "source $INSTALL_DIR/bin/setupvars.sh"

RUN echo "source $INSTALL_DIR/bin/setupvars.sh" >> /root/.bashrc

COPY policy.yml /etc/backend.ai/jail/policy.yml

# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "Intel OpenVINO 2019.3 on Backend.AI" && \
    cat /opt/conda/share/jupyter/kernels/python3/kernel.json

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

WORKDIR /home/work
# vim: ft=dockerfile

