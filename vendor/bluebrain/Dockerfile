FROM nvcr.io/hpc/pgi-compilers:ce

ENV PYTHONUNBUFFERED=1 \
    LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:/usr/local/cuda/lib64:/usr/local/nvidia/lib64:/usr/local/lib:$LD_LIBRARY_PATH \
    PATH=/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH \
    LANG=C.UTF-8

ENV CC=mpicc
ENV CXX=mpicxx

RUN apt-get update -y && \
    apt-get install -y \
    	gcc g++ make \
	gfortran \
	git \
	python3 python3-pip \
        xz-utils \
        wget curl git-core \
        vim-tiny bzip2 zip unzip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
	mpich \
	bison \
	flex \
        libssl-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/
	
WORKDIR /tmp
RUN wget https://github.com/Kitware/CMake/releases/download/v3.15.4/cmake-3.15.4.tar.gz && \
    tar xvf cmake-3.15.4.tar.gz  && \
    cd cmake-3.15.4/ && \
    ./configure && \
    make -j${nproc} && \
    make install && \
    rm /tmp/cmake-3.15.4.tar.gz

# python alternative support
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

WORKDIR /opt
RUN git clone https://github.com/BlueBrain/mod2c.git && \
    cd mod2c && \
    mkdir build && cd build && \
    cmake .. \
      -DCMAKE_C_FLAGS:STRING="-O2" \
      -DCMAKE_CXX_FLAGS:STRING="-O2" \
      -DCOMPILE_LIBRARY_TYPE=STATIC \
      -DCUDA_HOST_COMPILER=`which gcc` \
      -DCUDA_PROPAGATE_HOST_FLAGS=OFF \
      -DENABLE_SELECTIVE_GPU_PROFILING=ON \
      -DENABLE_OPENACC=ON \
    && \
    make -j${nproc} && \
    make install

WORKDIR /opt
RUN git clone --recursive https://github.com/BlueBrain/CoreNeuron.git && \
    cd CoreNeuron && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j${nproc} && \
    make install 
    
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir versioneer==0.17 && \
    python3 -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python3 -m pip install --no-cache-dir pandas && \
    python3 -m pip install --no-cache-dir seaborn && \
    python3 -m pip install --no-cache-dir pillow && \
    python3 -m pip install --no-cache-dir networkx cvxpy && \
    python3 -m pip install --no-cache-dir scikit-learn scikit-image && \
    python3 -m pip install --no-cache-dir pygments && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

COPY policy.yml /etc/backend.ai/jail/policy.yml

WORKDIR /home/work
# vim: ft=dockerfile

