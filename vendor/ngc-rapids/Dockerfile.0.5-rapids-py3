# The RAPIDS suite of software libraries gives you the freedom to execute end-to-end data science and analytics pipelines entirely on GPUs.

FROM nvcr.io/nvidia/rapidsai/rapidsai:0.5-cuda9.2-base-ubuntu16.04-gcc5-py3.6
MAINTAINER Mario Cho "m.cho@lablup.com"

ENV PYTHONUNBUFFERED=1 \
    LANG=C.UTF-8
RUN python3 -m pip install --no-cache-dir -U pip setuptools && \
    python3 -m pip install --no-cache-dir pillow && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab

# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "Python 3.6 (NGC/RAPIDS 0.5) on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="query batch uid-match" \
      ai.backend.accelerators="cuda" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="radips:http:5000,tensorboard:http:6006,ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

# vim: ft=dockerfile
