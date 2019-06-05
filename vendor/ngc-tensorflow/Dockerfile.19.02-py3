FROM nvcr.io/nvidia/tensorflow:19.02-py3
# NVIDIA DIGITS runs on Python 3.5

RUN apt-get update && \
    apt-get install -y libsm6 libxext6 libxrender-dev
ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8
RUN /usr/bin/python3 -m pip install -U pip setuptools && \
    /usr/bin/python3 -m pip install --no-cache-dir keras && \
    /usr/bin/python3 -m pip install --no-cache-dir keras_applications && \
    /usr/bin/python3 -m pip install --no-cache-dir keras_preprocessing && \
    /usr/bin/python3 -m pip install --no-cache-dir ipython && \
    /usr/bin/python3 -m pip install --no-cache-dir pillow && \
    /usr/bin/python3 -m pip install --no-cache-dir opencv-python && \
    /usr/bin/python3 -m pip install --no-cache-dir h5py && \
    /usr/bin/python3 -m pip install --no-cache-dir jupyter && \
    /usr/bin/python3 -m pip install --no-cache-dir jupyterlab

# Install ipython kernelspec
RUN /usr/bin/python3 -m ipykernel install --display-name "Python 3.5 (NGC/TensorFlow 19.02) on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# for apt-get installation using /tmp
RUN mkdir -p /tmp && \
    chown root:root /tmp && \
    chmod 1777 /tmp

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Backend.AI specifics
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch uid-match" \
      ai.backend.accelerators="cuda" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,tensorboard:http:6006,jupyter:http:8080,jupyterlab:http:8090"

# vim: ft=dockerfile
