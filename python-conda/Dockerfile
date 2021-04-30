FROM centos:7
LABEL maintainer "Mario Cho <m.cho@lablup.com>"

ENV DEBIAN_FRONTEND=noninteractive \
    MPLBACKEND=Svg \
    PYTHONUNBUFFERED=1 \
    LIBRARY_PATH=/usr/local/cuda/lib64/stubs \
    _CUDA_COMPAT_PATH="/usr/local/cuda/compat" \
    LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:/usr/local/cuda/compat/lib:/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/include/x86_64-linux-gnu"\
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/opt/conda/bin:/usr/local/sbin:/usr/bin/cmake/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/tensorrt/bin" \
    LANG=C.UTF-8

ENV CUDA_VER 10.1
ENV CUDA_VERSION 10.1.243
ENV CUDA_PKG_VERSION 10-1-$CUDA_VERSION-1
ENV NCCL_VERSION 2.8.4
ENV CUDNN_VERSION 7.6.5.32
LABEL com.nvidia.cudnn.version="${CUDNN_VERSION}"

# nvidia-container-runtime
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES compute,utility
ENV NVIDIA_REQUIRE_CUDA "cuda>=10.1 brand=tesla,driver>=396,driver<397 brand=tesla,driver>=410,driver<411 brand=tesla,driver>=418,driver<419"
ENV CUDA_HOME /usr/local/cuda

RUN yum update -y && \
    yum install -y epel-release && \
#    yum install -y https://download1.rpmfusion.org/free/el/rpmfusion-free-release-7.noarch.rpm && \
    yum install -y \
        atlas-devel \
	numactl-devel  \
	alsa-lib \
	cairo \
	cairo-devel \
	gcc-c++ \
	giflib-devel \
	lapack-devel \
	libjpeg-turbo-devel  \
	libtool \
	make \
	nano \
	numactl \
	numactl-libs \
	openblas-devel \
	openssh-server \
	pmix-devel \
	vim \
  	openssh-clients \
        autoconf \
        automake \
        bc \
        ca-certificates \
        cmake \
        curl \
        ffmpeg \
        flac \
        gawk \
        gcc-gfortran \
        git \
        libsndfile-devel \
        openssl-devel \
        python3 \
        sox \
        subversion \
        unzip \
        wget \
        zip \
        zlib && \
    curl -sL https://rpm.nodesource.com/setup_14.x | bash - && \
    yum install -y nodejs

RUN NVIDIA_GPGKEY_SUM=d1be581509378368edeec8c1eb2958702feedf3bc3d17011adbf24efacce4ab5 && \
    curl -fsSL https://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/7fa2af80.pub | sed '/^Version/d' > /etc/pki/rpm-gpg/RPM-GPG-KEY-NVIDIA && \
    echo "$NVIDIA_GPGKEY_SUM  /etc/pki/rpm-gpg/RPM-GPG-KEY-NVIDIA" | sha256sum -c --strict  -

COPY cuda.repo /etc/yum.repos.d/cuda.repo
COPY nvidia-ml.repo /etc/yum.repos.d/nvidia-ml.repo
#COPY NGC-DL-CONTAINER-LICENSE /

RUN yum upgrade -y && yum install -y \
    cuda-cudart-$CUDA_PKG_VERSION \
    cuda-compat-10-1 && \
    ln -s cuda-10.1 /usr/local/cuda && \
    echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf && \
    echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf


# setopt flag prevents yum from auto upgrading. See https://gitlab.com/nvidia/container-images/cuda/-/issues/88
RUN yum install --setopt=obsoletes=0 -y \
    cuda-libraries-$CUDA_PKG_VERSION \
    cuda-nvtx-$CUDA_PKG_VERSION \
    cuda-npp-$CUDA_PKG_VERSION \
    libcublas10-10.2.1.243-1 \
    && yum clean all \
    && rm -rf /var/cache/yum/*

RUN yum install -y yum-plugin-versionlock && yum versionlock libcublas10

# setopt flag prevents yum from auto upgrading. See https://gitlab.com/nvidia/container-images/cuda/-/issues/88
RUN yum install --setopt=obsoletes=0 -y \
    make \
    cuda-nvml-dev-$CUDA_PKG_VERSION \
    cuda-command-line-tools-$CUDA_PKG_VERSION \
    libcublas-devel-10.2.1.243-1 \
    cuda-cudart-dev-$CUDA_PKG_VERSION \
    cuda-libraries-dev-$CUDA_PKG_VERSION \
    cuda-minimal-build-$CUDA_PKG_VERSION \
    cuda-nvprof-$CUDA_PKG_VERSION \
    cuda-npp-dev-$CUDA_PKG_VERSION \
    && yum clean all \
    && rm -rf /var/cache/yum/*

RUN yum install -y yum-plugin-versionlock && yum versionlock libcublas10

RUN yum install -y xz && NCCL_DOWNLOAD_SUM=bcff1cf98e4b24d7ca189577a9d909980d8df88075223d70dc4638e428c53f84 && \
    curl -fsSL https://developer.download.nvidia.com/compute/redist/nccl/v2.8/nccl_2.8.4-1+cuda10.1_x86_64.txz -O && \
    echo "$NCCL_DOWNLOAD_SUM  nccl_2.8.4-1+cuda10.1_x86_64.txz" | sha256sum -c - && \
    unxz nccl_2.8.4-1+cuda10.1_x86_64.txz && \
    tar --no-same-owner --keep-old-files -xvf  nccl_2.8.4-1+cuda10.1_x86_64.tar -C /usr/local/cuda/include/ --strip-components=2 --wildcards '*/include/*' && \
    tar --no-same-owner --keep-old-files -xvf  nccl_2.8.4-1+cuda10.1_x86_64.tar -C /usr/local/cuda/lib64/ --strip-components=2 --wildcards '*/lib/libnccl.so' && \
    rm -f nccl_2.8.4-1+cuda10.1_x86_64.tar && \
    ldconfig


# cuDNN license: https://developer.nvidia.com/cudnn/license_agreement
RUN CUDNN_DOWNLOAD_SUM=7eaec8039a2c30ab0bc758d303588767693def6bf49b22485a2c00bf2e136cb3 && \
    curl -fsSL http://developer.download.nvidia.com/compute/redist/cudnn/v7.6.5/cudnn-10.1-linux-x64-v7.6.5.32.tgz -O && \
    echo "$CUDNN_DOWNLOAD_SUM  cudnn-10.1-linux-x64-v7.6.5.32.tgz" | sha256sum -c - && \
    gunzip cudnn-10.1-linux-x64-v7.6.5.32.tgz && \
    tar --no-same-owner -xf cudnn-10.1-linux-x64-v7.6.5.32.tar -C /usr/local --wildcards 'cuda/lib64/libcudnn.so.*' && \
    rm cudnn-10.1-linux-x64-v7.6.5.32.tar && \
    ldconfig


# Install miniconda 3.8
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-py38_4.9.2-Linux-x86_64.sh -O miniconda.sh && \
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

RUN /opt/conda/bin/conda install \
	    pytorch \
	    torchvision \
	    torchaudio \
	    cudatoolkit=10.1 \
	    -c pytorch \
	    -c conda-forge
RUN /opt/conda/bin/python3 -m pip install --no-cache-dir \
    	    Cython==0.29.22 \
	    tornado==6.1 \
	    pystan==3.0.1 \
	    pycairo==1.20.0 \
	    numpy==1.20.2 \
	    tensorflow==2.3.2 \
	    tensorboard==2.3.0 \
	    scipy==1.6.3 \
	    scikit-learn==0.24.2 \
	    jupyter==1.0.0 \
	    typeguard==2.11.1 \
	    python-language-server[all] \	    
	    matplotlib==3.4.1
	    
WORKDIR /tmp
COPY ./requirements.txt /tmp
RUN /opt/conda/bin/python3 -m pip install --no-cache-dir --ignore-installed -r requirements.txt && \
    rm -f /tmp/*.whl /tmp/requirements.txt

# Install Jupyterlab extensions
RUN jupyter nbextensions_configurator enable && \
    jupyter contrib nbextension install && \
    jupyter nbextension enable --py --sys-prefix widgetsnbextension && \
    jupyter serverextension enable --py jupyter_lsp && \
    jupyter nbextension enable execute_time/ExecuteTime && \
    jupyter nbextension enable toc2/main

# Copy Backend.Ai multi-node support
COPY ./runner-scripts/bootstrap.sh /opt/container/
COPY ./service-defs /etc/backend.ai/service-defs
# Install ipython kernelspec
Run /opt/conda/bin/python3 -m ipykernel install --display-name "Conda python 3.8 on CUDA 10.1 & CentOS 7" && \
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
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8081,vscode:http:8180,tensorboard:http:6006"

WORKDIR /home/work
# vim: ft=dockerfile
