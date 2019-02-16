FROM lablup/kernel-base:python3.6 as python-binary

FROM nvidia/cuda:9.0-cudnn7-runtime-ubuntu16.04
MAINTAINER Mario Cho "m.cho@lablup.com"
ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1
ENV CUDA_VERSION 9.0.176
ENV CUDA_PKG_VERSION 9-0=$CUDA_VERSION-1
ENV CUDNN_VERSION 7.3.1.20
ENV NCCL_VERSION 2.3.5
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
ENV CUDA_HOME /usr/local/cuda
ENV CUDA_TOOLKIT_PATH /usr/local/cuda
ENV CUDNN_INSTALL_PATH /usr/local/cuda

RUN apt update -y && \
    apt install -y \
        ca-certificates \
        wget \
        libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g \
	libmpdec2 \
        libssl1.0.0 \
	libssl-dev \
        libncursesw5 libtinfo5 libreadline6 \
	proj-bin \
        libgeos-dev \
        mime-support \
	git \
	gcc g++ \
	cmake \
        libproj-dev libgeos-dev \	
        libzmq3-dev libuv1

COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
# Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

# Install CUDA-9.0.176 + cuDNN 7.3.1.20
RUN apt-get update && apt-get install -y --no-install-recommends \
        cuda-libraries-$CUDA_PKG_VERSION \
        cuda-cublas-9-0=9.0.176.4-1 \
        libnccl2=$NCCL_VERSION-2+cuda9.0 && \
    apt-mark hold libnccl2 && \
    rm -rf /var/lib/apt/lists/* 

RUN ln -s /usr/local/cuda-9.0 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.2.1 /usr/local/cuda/lib64/libcudnn.so && \
    ldconfig
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

RUN pip install --no-cache-dir \
        pyyaml

# Install Torch
RUN git clone --recursive https://github.com/pytorch/pytorch && \
    cd pytorch && \
    export USE_CUDA=0 && \
    export TORCH_CUDA_ARCH_LIST="3.5;5.0+PTX;6.0;6.1;7.0" && \
    export TORCH_NVCC_FLAGS="-Xfatbin -compress-all" && \
    export CUDA_HOME=/usr/local/cuda && \
    export CUDA_TOOLKIT_PATH=/usr/local/cuda && \
    export CUDNN_INSTALL_PATH=/usr/local/cuda && \
    export NCCL_ROOT_DIR=/usr/local/cuda  && \
    export LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" && \
    export PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" && \
    python setup.py bdist_wheel -d /tmp 

RUN git clone --recursive https://github.com/pytorch/vision.git && \
    cd vision && \
    python setup.py bdist_wheel -d /tmp 
