FROM nvcr.io/nvidia/caffe2:18.08-py3
# NVIDIA Caffe2 with Python 3.6 (CONDA)

ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8

# ipython/jupyter is already installed in the Anaconda Python installed at /opt/conda.
RUN /usr/bin/python3 -m pip install ipython && \
    /usr/bin/python3 -m pip install jupyter && \
    /usr/bin/python3 -m pip install jupyterlab && \
    /usr/bin/python3 -m pip install opencv-python 

# Install ipython kernelspec
RUN /usr/bin/python3 -m ipykernel install \
        --display-name "Python 3.6 (NGC/Caffe3 18.08) on Backend.AI" && \
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
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

# vim: ft=dockerfile
