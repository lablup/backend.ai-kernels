FROM ubuntu:16.04
MAINTAINER caffe-maint@googlegroups.com

#ENV http_proxy proxy:port
#ENV https_proxy proxy:port

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
	bzip2 \
        build-essential \
        cmake \
        git \
        wget \
        ssh \
        openssh-server \
        numactl \
        vim \
        net-tools \
        iputils-ping \
        ufw \
        iptables && \
    rm -rf /var/lib/apt/lists/*

# Install conda and Intel Caffe conda package
WORKDIR /tmp
RUN wget --no-check-certificate -c https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh && \
    bash Miniconda2-latest-Linux-x86_64.sh -b -p /opt/conda && \
    /opt/conda/bin/conda config --add channels intel && \
    /opt/conda/bin/conda install -c intel caffe && \
    /opt/conda/bin/conda install -y -q intelpython3_full=2019.4 python=3 && \
    /opt/conda/bin/conda install -c anaconda opencv && \
    /opt/conda/bin/conda install -c conda-forge pycocotools && \
    /opt/conda/bin/conda install -c conda-forge nodejs && \
    /opt/conda/bin/conda install -c conda-forge jupyterlab && \
    /opt/conda/bin/conda install -c conda-forge jupyterthemes && \
    /opt/conda/bin/conda install -c conda-forge ipywidgets && \
    /opt/conda/bin/conda install -c conda-forge jupyter_contrib_nbextensions && \
    /opt/conda/bin/conda install -c conda-forge jupyter_nbextensions_configurator && \
    /opt/conda/bin/conda clean --all && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl && \
    rm -rf /opt/conda/pkgs/* && \
    rm /tmp/Miniconda2-latest-Linux-x86_64.sh -f && \
    echo "export PATH=/opt/conda/bin:$PATH" >> /root/.bashrc

WORKDIR /home/work

ENV NOTVISIBLE "in users profile"
RUN echo "export VISIBLE=now" >> /etc/profile

ENV PYTHONUNBUFFERED=1 \
    LIBRARY_PATH="/opt/conda/lib/libfabric" \
    PATH=/opt/conda/bin/libfabric:/opt/conda/bin:/opt/conda/condabin:/opt/conda/bin:$PATH \
    LANG=C.UTF-8 \
    CLASSPATH="/opt/conda/lib/mpi.jar" \
    CONDA_DEFAULT_ENV="base" \
    CONDA_EXE="/opt/conda/bin/conda" \
    CONDA_PREFIX="/opt/conda" \
    CONDA_PROMPT_MODIFIER="(base) " \
    CONDA_PYTHON_EXE="/opt/conda/bin/python" \
    CONDA_SHLVL="1" \
    FI_PROVIDER_PATH="/opt/conda/lib/libfabric/prov" \
    HOME="/home/work" \
    I_MPI_ROOT="/opt/conda" \
    LC_ALL="C.UTF-8" \
    LD_LIBRARY_PATH="/opt/conda/lib/libfabric:/opt/conda/lib" \
    MANPATH="/opt/conda/share/man:"

# Jupyter notebook extension
RUN mkdir -p /home/work/.jupyter/nbextension
WORKDIR /home/work/.jupyter/nbextension

RUN jupyter nbextensions_configurator enable && \
    jupyter contrib nbextension install && \
    jupyter nbextension enable --py --sys-prefix widgetsnbextension && \
    jupyter contrib nbextension install && \
    jupyter serverextension enable --py jupyterlab --sys-prefix && \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager && \
    git clone https://github.com/lambdalisue/jupyter-vim-binding vim_binding && \
    jupyter nbextension enable /home/work/.jupyter/nbextension/vim_binding/vim_binding
    
# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "Intel Caffe & Python 3.6 on Backend.AI" && \
    cat /opt/conda/share/jupyter/kernels/python3/kernel.json

COPY policy.yml /etc/backend.ai/jail/policy.yml

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/opt/conda/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

WORKDIR /home/work
# vim: ft=dockerfile
