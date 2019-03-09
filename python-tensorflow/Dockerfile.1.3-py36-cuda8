FROM lablup/common-tensorflow:1.3-py36-cuda8 as tf-binary
FROM nvidia/cuda:8.0-cudnn6-runtime-ubuntu16.04
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        vim-tiny zip unzip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        gcc g++ make \
        xz-utils \
        libsqlite3-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

#install Python 3
RUN curl https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tar.xz -o /opt/python.tar.xz && \
      cd /opt && tar xvf python.tar.xz && \
      cd /opt/*/ && ./configure && \
      make && make install && \
      ln -s -f /usr/local/bin/python3.6 /usr/bin/python

RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py && \
    python get-pip.py && \
    rm get-pip.py

ENV PYTHONUNBUFFERED=1 \
    LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" \
    LANG=C.UTF-8

# Install CUDA-8.0 + cuDNN 6.0
RUN ln -s /usr/local/cuda-8.0 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.6.0.21 /usr/local/cuda/lib64/libcudnn.so && \
    ldconfig


# As we mostly have "manylinux" glibc-compatible binary packaes,
# we don't have to rebuild these!
COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp

RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools && \
    python3 -m pip install --no-cache-dir wheel && \
    python3 -m pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    python3 -m pip install --no-cache-dir aiozmq dataclasses tabulate && \
    python3 -m pip install --no-cache-dir namedlist six "python-dateutil>=2" && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir numpy scipy && \
    python3 -m pip install --no-cache-dir versioneer==0.17 && \
    python3 -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir pandas && \
    python3 -m pip install --no-cache-dir seaborn && \
    python3 -m pip install --no-cache-dir pillow && \
    python3 -m pip install --no-cache-dir networkx cvxpy && \
    python3 -m pip install --no-cache-dir scikit-learn scikit-image && \
    python3 -m pip install --no-cache-dir pygments && \
    python3 -m pip install --no-cache-dir wheel /tmp/*.whl && \
    python3 -m pip install --no-cache-dir keras && \
    python3 -m pip install --no-cache-dir keras_applications && \
    python3 -m pip install --no-cache-dir keras_preprocessing && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "TensorFlow 1.3 on Python 3.6 (CUDA 8.0)" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

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
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile

