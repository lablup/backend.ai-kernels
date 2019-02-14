FROM ubuntu:18.04
# Ubuntu 18.04 comes with Python 3.6

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        python3 python3-pip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        gcc g++ && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PATH=/opt/backend.ai/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive

# Install commonly-used wheels
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab

RUN apt update && \
    apt install -y software-properties-common && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' && \
    apt update && \
    apt install -y r-base

RUN Rscript -e "install.packages('IRkernel')" && \
    Rscript -e "IRkernel::installspec()" && \
    mv /root/.local/share/jupyter/kernels/ir /usr/local/share/jupyter/kernels && \
    rm -rf /root/.local/share/jupyter

LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="r" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="jupyter:http:8080,jupyterlab:http:8090"
COPY policy.yml /etc/backend.ai/jail/policy.yml
