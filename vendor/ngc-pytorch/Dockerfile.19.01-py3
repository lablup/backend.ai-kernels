FROM nvcr.io/nvidia/pytorch:19.01-py3
# NVIDIA PyTorch with Python 3.6 (CONDA)

ENV PYTHONUNBUFFERED=1 \
    PATH=/opt/conda/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8

# ipython/jupyter is already installed in the Anaconda Python installed at /opt/conda.
RUN /opt/conda/bin/python -m pip install jupyterlab

# Install ipython kernelspec
RUN /opt/conda/bin/python -m ipykernel install \
        --prefix=/opt/conda/ \
        --display-name "Python 3.6 CONDA (NGC/PyTorch 19.01) on Backend.AI" && \
    cat /opt/conda/share/jupyter/kernels/python3/kernel.json

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
      ai.backend.runtime-path="/opt/conda/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

# vim: ft=dockerfile
