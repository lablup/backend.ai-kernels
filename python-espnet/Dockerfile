FROM ubuntu:18.04
LABEL maintainer "Mario Cho <m.cho@lablup.com>"

ENV DEBIAN_FRONTEND=noninteractive \
    MPLBACKEND=Svg \
    PYTHONUNBUFFERED=1 \
    LIBRARY_PATH=/usr/local/cuda/lib64/stubs \
    _CUDA_COMPAT_PATH="/usr/local/cuda/compat" \
    LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:/usr/local/cuda/compat/lib:/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/include/x86_64-linux-gnu"\
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/opt/conda/bin:/usr/local/sbin:/usr/bin/cmake/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/tensorrt/bin" \
    LANG=C.UTF-8

ENV CUDA_VERSION 11.0.3
ENV CUDA_PKG_VERSION 11-0=$CUDA_VERSION-1
ENV NCCL_VERSION 2.8.4
ENV CUDNN_VERSION 8.0.5.39
LABEL com.nvidia.cudnn.version="${CUDNN_VERSION}"

# nvidia-container-runtime
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES compute,utility
ENV NVIDIA_REQUIRE_CUDA "cuda>=11.0 brand=tesla,driver>=418,driver<419 brand=tesla,driver>=440,driver<441 brand=tesla,driver>=450,driver<451"
ENV CUDA_HOME /usr/local/cuda

RUN apt-get update && \
    apt-get -y install --no-install-recommends \ 
	fonts-nanum \
	fonts-nanum-coding \
	fonts-nanum-extra \
	libasound-dev \
	libcairo2-dev \
	libgif-dev \
	libjpeg-dev  \
	liblapack-dev \
	libnuma-dev \
	libopenblas-dev \
	libpmi2-0-dev \
	nano \
	numactl \
	vim \
  	openssh-client openssh-server \
        apt-utils \
        autoconf \
        automake \
        bc \
        build-essential \
        ca-certificates \
        cmake \
        curl \
	debhelper dh-virtualenv \
        ffmpeg \
        flac \
        gawk \
        gfortran \
        git \
        libatlas-base-dev \
        libatlas3-base libtool \
        libsndfile1-dev \
        libssl-dev \
        libtool \
        python2.7 \
        python3 \
        sox \
        subversion \
        unzip \
        wget \
        zip \
        zlib1g-dev \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

## FROM CUDA 11.0 base [https://gitlab.com/nvidia/cuda/blob/ubuntu18.04/11.0/base/Dockerfile] 

RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg2 curl ca-certificates && \
    curl -fsSL https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/7fa2af80.pub | apt-key add - && \
    echo "deb https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64 /" > /etc/apt/sources.list.d/cuda.list && \
    echo "deb https://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1804/x86_64 /" > /etc/apt/sources.list.d/nvidia-ml.list && \
    rm -rf /var/lib/apt/lists/*

# For libraries in the cuda-compat-* package: https://docs.nvidia.com/cuda/eula/index.html#attachment-a
RUN apt-get update && apt-get install -y --no-install-recommends \
        cuda-cudart-11-0=11.0.221-1 \
	cuda-compat-11-0 && \
    ln -s cuda-11.0 /usr/local/cuda && \
    rm -rf /var/lib/apt/lists/*

# Required for nvidia-docker v1
RUN echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf && \
    echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf

## FROM CUDA 11.0 runtime [https://gitlab.com/nvidia/cuda/blob/ubuntu18.04/11.0/runtime/Dockerfile]

RUN apt-get update && apt-get install -y --no-install-recommends \
    	cuda-libraries-11-0=11.0.3-1 \
	libnpp-11-0=11.1.0.245-1 \
	cuda-nvtx-11-0=11.0.167-1 \
	libcublas-11-0=11.2.0.252-1 \
	libcusparse-11-0=11.1.1.245-1 \
	libnccl2=$NCCL_VERSION-1+cuda11.0 \
	&& \
    apt-mark hold libcublas-11-0 libnccl2 && \
    rm -rf /var/lib/apt/lists/*

## FROM CUDA 11.0 devel [https://gitlab.com/nvidia/cuda/blob/ubuntu18.04/11.0/devel/Dockerfile]

RUN apt-get update && apt-get install -y --no-install-recommends \
    	cuda-cudart-dev-11-0=11.0.221-1 \
	cuda-command-line-tools-11-0=11.0.3-1 \
	cuda-minimal-build-11-0=11.0.3-1 \
	cuda-libraries-dev-11-0=11.0.3-1 \
	cuda-nvml-dev-11-0=11.0.167-1 \
	libnpp-dev-11-0=11.1.0.245-1 \
	libnccl-dev=2.8.4-1+cuda11.0 \
	libcublas-dev-11-0=11.2.0.252-1 \
	libcusparse-dev-11-0=11.1.1.245-1 \
	&& \
    apt-mark hold libcublas-dev-11-0 libnccl-dev &&  \
    rm -rf /var/lib/apt/lists/*

## FROM CUDA 11.0-CUDNN 8 devel

RUN apt-get update && apt-get install -y --no-install-recommends \
        libcudnn8=$CUDNN_VERSION-1+cuda11.0 \
        libcudnn8-dev=$CUDNN_VERSION-1+cuda11.0 && \
    apt-mark hold libcudnn8 && \
    rm -rf /var/lib/apt/lists/*

RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-py37_4.9.2-Linux-x86_64.sh -O miniconda.sh && \
    mkdir -p /opt && \
    sh miniconda.sh -b -p /opt/conda && \
    rm miniconda.sh && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc && \
    find /opt/conda/ -follow -type f -name '*.a' -delete && \
    find /opt/conda/ -follow -type f -name '*.js.map' -delete && \
    /opt/conda/bin/conda clean -afy

# Install OpenBlas
WORKDIR /tmp
RUN git clone -q --branch=master git://github.com/xianyi/OpenBLAS.git && \
    cd OpenBLAS && \
    make DYNAMIC_ARCH=1 NO_AFFINITY=1 NUM_THREADS=48 FC=gfortran && \
    make install

# Install Open UCX
WORKDIR /tmp
RUN wget https://github.com/openucx/ucx/archive/v1.9.0.tar.gz && \
    tar xvf v1.9.0.tar.gz && \
    cd ucx-1.9.0 && \
    bash ./autogen.sh && \
    mkdir build && \
    cd build && \
    ../configure --prefix=/usr/local/ucx && \
    make -j$(nproc) && \
    make install 

# Install Open MPI
RUN mkdir /tmp/openmpi && \
    cd /tmp/openmpi && \
    wget https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.0.tar.gz && \
    tar zxf openmpi-4.1.0.tar.gz && \
    cd openmpi-4.1.0 && \
    ./configure --enable-orterun-prefix-by-default --with-cuda --with-ucx=/usr/local/ucx --enable-mca-no-build=btl-uct && \
    make -j $(nproc) all && \
    make install && \
    ldconfig && \
    rm -rf /tmp/openmpi*

# Allow OpenSSH to talk to containers without asking for confirmation
RUN mkdir -p /var/run/sshd && \
    cat /etc/ssh/ssh_config | grep -v StrictHostKeyChecking > /etc/ssh/ssh_config.new && \
    echo "    StrictHostKeyChecking no" >> /etc/ssh/ssh_config.new && \
    mv /etc/ssh/ssh_config.new /etc/ssh/ssh_config

# Create a wrapper for OpenMPI to allow running as root by default
RUN mv /usr/local/bin/mpirun /usr/local/bin/mpirun.real && \
    echo '#!/bin/bash' > /usr/local/bin/mpirun && \
    echo 'mpirun.real --allow-run-as-root "$@"' >> /usr/local/bin/mpirun && \
    chmod a+x /usr/local/bin/mpirun

# Configure OpenMPI to run good defaults:
RUN echo "btl_tcp_if_exclude = lo,docker0" >> /usr/local/etc/openmpi-mca-params.conf

# Install git-lfs
WORKDIR /tmp
RUN curl -sLO https://github.com/git-lfs/git-lfs/releases/download/v2.13.1/git-lfs-linux-amd64-v2.13.1.tar.gz && \
    tar -zxf git-lfs-linux-amd64-v2.13.1.tar.gz && \
    bash install.sh && \
    rm -rf /tmp/*

# Install VSCode
RUN curl -fL https://github.com/cdr/code-server/releases/download/v3.8.0/code-server-3.8.0-linux-amd64.tar.gz \
  | tar -C /usr/local/lib -xz && \
    mv /usr/local/lib/code-server-3.8.0-linux-amd64 /usr/local/lib/code-server-3.8.0 && \
    ln -s /usr/local/lib/code-server-3.8.0/bin/code-server /usr/local/bin/code-server

# Install Kaldi
RUN git clone https://github.com/hephaex/kaldi.git /opt/kaldi && \
    cd /opt/kaldi/tools && \
    ./extras/install_mkl.sh -sp debian intel-mkl-64bit-2019.2-057 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    make -j $(nproc) all && \
    rm -r openfst-*/src && \
    ./extras/install_beamformit.sh && \
    ./extras/install_irstlm.sh && \
    cd /opt/kaldi/src && \
    ./configure --shared --use-cuda \
	--cudatk-dir=/usr/local/cuda/ \
	--mathlib=ATLAS \
    	--cuda-arch="-gencode arch=compute_52,code=sm_52 -gencode arch=compute_60,code=sm_60 -gencode arch=compute_61,code=sm_61 -gencode arch=compute_70,code=sm_70 -gencode arch=compute_75,code=sm_75 -gencode arch=compute_80,code=sm_80" && \
    make depend -j $(nproc) && \
    make -j $(nproc) && \
    find /opt/kaldi/src -name "*.o" -exec rm -f {} \; && \
    find /opt/kaldi/src -name "*.o" -exec rm -f {} \;

# Install espnet
ENV TH_VERSION 1.7.1
ENV CUDA_VER 11.0
RUN git clone https://github.com/espnet/espnet /opt/espnet
WORKDIR /opt/espnet/tools

# Replace nvidia-smi for nvcc because docker does not load nvidia-smi
RUN if [ -z "$( which nvcc )" ]; then \
        echo "Build without CUDA" && \
        MY_OPTS='CUPY_VERSION=""  TH_VERSION=1.7.1'; \
    else \
        echo "Build with CUDA" && \
        # Docker containers cannot load cuda libs during build.
        # So, their checks on cuda packages are disabled.
        sed -i '200s|install.py|install.py --no-cuda --no-cupy |' Makefile && \
        export CFLAGS="-I${CUDA_HOME}/include ${CFLAGS}" && \
        MY_OPTS="CUDA_VERSION=${CUDA_VER}" && \
        . ./setup_cuda_env.sh /usr/local/cuda;  \
    fi; \ 
    if [ "${CUDA_VER}" = "11.0" ]; then \
        # warpctc is not supported from Pytorch 1.3.1
        MY_OPTS="${MY_OPTS} TH_VERSION=1.7.1";  \
    fi; \
    echo "Make with options ${MY_OPTS}" && \
    ln -s /opt/kaldi ./ && \
    ./setup_python.sh /opt/conda/bin/python3 && \
    make KALDI=/opt/kaldi ${MY_OPTS}

# Add extra libraries (VC/TTS)
WORKDIR /opt/espnet/tools
RUN /opt/conda/bin/python3 -m pip install --no-cache-dir \
        torch==1.7.1 parallel_wavegan && \
    /opt/conda/bin/python3 -m pip install --no-cache-dir \
        torch==1.7.1 git+https://github.com/cybertronai/pytorch-lamb

RUN /opt/conda/bin/python3 -m pip install --no-cache-dir \
    	    git+https://github.com/lanpa/tensorboardX \
	    tornado==6.0.4 \
	    pystan==2.19.1.1 \
	    pycairo==1.19.0 \
	    jupyter==1.0.0 \
	    python-language-server[all] 

# Copy Backend.Ai multi-node support
COPY runner-scripts/bootstrap.sh /opt/container/
COPY ./service-defs /etc/backend.ai/service-defs
RUN echo "source /opt/espnet/tools/activate_python.sh" >> /etc/profile && \
    chmod +x /opt/espnet/tools/activate_python.sh && \
    chmod -R 777 /opt/espnet 

# Install ipython kernelspec
Run /opt/conda/bin/python3 -m ipykernel install --display-name "A4003 (ESPNet, PyTorch 1.7.1 on Python 3.7 & CUDA 11.0)" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json


# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=0 \
      ai.backend.resource.min.cuda.shares=0 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/opt/conda/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8081,jupyterlab:http:8090,vscode:http:8180,tensorboard:http:6006"


WORKDIR /home/work
# vim: ft=dockerfile
