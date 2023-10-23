FROM ubuntu:22.04
# Ubuntu 22.04 comes with Python 3.10

ARG MODULAR_AUTH
ENV DEBIAN_FRONTEND noninteractive \
    DEFAUL_TZ=Asia/Seoul \
    PYTHONUNBUFFERED=1 \
    LLVM_SYMBOLIZER_PATH=/usr/bin/llvm-symbolizer \
    LANG=C.UTF-8

WORKDIR /tmp
RUN apt update && \
    apt install -y \
        apt-transport-https \
        wget curl git-core \
    	ca-certificates \
	llvm curl git make \
        ncurses-term \
	python3.10-venv \
        vim-tiny zip unzip && \
    ln -sf /usr/share/terminfo/x/xterm-color /usr/share/terminfo/x/xterm-256color && \
# Install BashTop
    git clone https://github.com/aristocratos/bashtop.git && \
    cd bashtop && \
    make install && \
    cd /tmp && \
    curl -fL https://github.com/cdr/code-server/releases/download/v4.18.0/code-server-4.18.0-linux-amd64.tar.gz \
      | tar -C /usr/local/lib -xz && \ 
    mv /usr/local/lib/code-server-4.18.0-linux-amd64 /usr/local/lib/code-server-4.18.0 && \
    ln -s /usr/local/lib/code-server-4.18.0/bin/code-server /usr/local/bin/code-server 

# Install Modular CLI / Mojo
RUN curl https://get.modular.com |  sh -  && \
    mkdir -p /root/.modular && \
    ln -s /root/.modular /opt/mojo && \
    modular auth ${MODULAR_AUTH} && \
    modular install mojo && \
    chmod -R a+rwX /opt/mojo 

ENV MODULAR_HOME=/opt/mojo \
    PATH=/opt/mojo/pkg/packages.modular.com_mojo/bin:$PATH \
    MOJO_PYTHON_LIBRARY=/usr/lib/x86_64-linux-gnu/libpython3.10.so 

RUN python3 -m pip install --no-cache-dir h5py \
    	    Cython \
    	    matplotlib bokeh \
	    versioneer \
	    pyproj cartopy \
	    pandas \
	    seaborn \
	    pillow \
	    networkx cvxpy \
	    scikit-learn scikit-image \
	    pygments \
	    ipython \
	    jupyter \
	    jupyterlab && \
    jupyter labextension disable "@jupyterlab/apputils-extension:announcements" && \
# Install ipython kernelspec
    python3 -m ipykernel install --display-name "Mojo on Backend.AI" && \
    cat /opt/mojo/pkg/packages.modular.com_mojo/venv/share/jupyter/kernels/python3/kernel.json && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/ && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# Backend.AI specifics
COPY ./bootstrap.sh /opt/container/bootstrap.sh
COPY ./policy.yml /etc/backend.ai/jail/policy.yml
COPY ./service-defs /etc/backend.ai/service-defs
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu22.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8070,jupyterlab:http:8090,vscode:http:8180"

WORKDIR /home/work
# vim: ft=dockerfile
