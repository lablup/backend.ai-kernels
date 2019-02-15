# Microsoft CNTK 2.5.1 (CUDA9) & Python 3.6
# ref: https://docs.microsoft.com/en-us/cognitive-toolkit/Setup-Linux-Python

# mount from pre-builded common-cntk:2.5-py36-cuda9 as wheel binary.
FROM lablup/common-cntk:2.5-py36-cuda9 as cntk-binary

# base image
FROM nvidia/cuda:9.2-cudnn7-runtime-ubuntu18.04
MAINTAINER Mario Cho "m.cho@lablup.com"

# Install CNTK as the default backend for Keras
ENV KERAS_BACKEND=cntk

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        vim-tiny zip unzip \
        python3 python3-pip \
	openmpi-bin \
	libopenmpi2 \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support && \
    ln -s /usr/lib/x86_64-linux-gnu/libmpi_cxx.so.20 /usr/lib/x86_64-linux-gnu/libmpi_cxx.so.1 && \
    ln -s /usr/lib/x86_64-linux-gnu/libmpi.so.20.10.1 /usr/lib/x86_64-linux-gnu/libmpi.so.12 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PYTHONUNBUFFERED=1 \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" \
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/nvidia/lib64:/usr/local/openmpi/lib \
    LANG=C.UTF-8

# Install CUDA-9.2 + cuDNN 7.4
RUN ln -s /usr/local/cuda-9.2 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.4.1 /usr/local/cuda/lib64/libcudnn.so && \
    ldconfig

COPY --from=cntk-binary /tmp/*.whl /tmp

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
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# verification CNTK
#RUN python -c "import cntk; print(cntk.__version__)"

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "CNTK 2.5.1 on Python 3.6 (CUDA 9.2)" && \
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
