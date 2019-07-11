FROM ubuntu:18.04

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        vim-tiny zip unzip \
        python python-pip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        gcc g++ && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8

RUN curl https://bootstrap.pypa.io/get-pip.py | python && \
    python -m pip install --no-cache-dir -U setuptools && \
    python -m pip install --no-cache-dir h5py && \
    python -m pip install --no-cache-dir Cython && \
    python -m pip install --no-cache-dir matplotlib bokeh && \
    python -m pip install --no-cache-dir versioneer==0.17 && \
    python -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python -m pip install --no-cache-dir pandas && \
    python -m pip install --no-cache-dir seaborn && \
    python -m pip install --no-cache-dir pillow && \
    python -m pip install --no-cache-dir networkx cvxpy && \
    python -m pip install --no-cache-dir scikit-learn scikit-image && \
    python -m pip install --no-cache-dir pygments && \
    python -m pip install --no-cache-dir ipython && \
    python -m pip install --no-cache-dir jupyter && \
    python -m pip install --no-cache-dir jupyterlab && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "Python 2.7 on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python2/kernel.json

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile
