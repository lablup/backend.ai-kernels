FROM nvcr.io/nvidia/digits:19.01-tensorflow
# NVIDIA DIGITS runs on Python 2.7

ENV PYTHONUNBUFFERED=1 \
    LANG=C.UTF-8
RUN python -m pip install --no-cache-dir -U pip setuptools && \
    python -m pip install --no-cache-dir keras && \
    python -m pip install --no-cache-dir keras_applications && \
    python -m pip install --no-cache-dir keras_preprocessing && \
    python -m pip install --no-cache-dir pillow && \
    python -m pip install --no-cache-dir h5py && \
    python -m pip install --no-cache-dir ipython && \
    python -m pip install --no-cache-dir jupyter && \
    python -m pip install --no-cache-dir jupyterlab

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "Python 2.7 (NGC/DIGITS 19.01-tensorflow) on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python2/kernel.json

# for apt-get installation using /tmp
RUN mkdir -p /tmp && \
    chown root:root /tmp && \
    chmod 1777 /tmp

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Change DIGITS to use /home/work
ENV DIGITS_JOBS_DIR=/home/work/jobs \
    DIGITS_LOGFILE_FILENAME=/home/work/jobs/digits.log

COPY policy.yml /etc/backend.ai/jail/policy.yml
# service:ort 
# ipython:3000, digit:5000 ,tensorboard:6006, jupyter:8080, jupyterlab:8090
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
      ai.backend.service-ports="digits:http:5000,tensorboard:http:6006,ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

# vim: ft=dockerfile
