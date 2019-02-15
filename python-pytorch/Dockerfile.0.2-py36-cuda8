FROM lablup/kernel-base:python3.6 as python-binary
FROM nvidia/cuda:8.0-cudnn7-runtime-ubuntu16.04

ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt update -y && \
    apt install -y \
        ca-certificates \
        wget \
        libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g \
    libmpdec2 \
        libssl1.0.0 \
    libssl-dev \
        libncursesw5 libtinfo5 libreadline6 \
    proj-bin \
        libgeos-dev \
        mime-support \
    gcc g++ \
        libproj-dev libgeos-dev \
        libzmq3-dev libuv1

COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
# Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

# Install CUDA-8.0 + cuDNN 7.3.1
RUN ln -s /usr/local/cuda-8.0 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.2.1 /usr/local/cuda/lib64/libcudnn.so && \
    ldconfig
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

# Prepare for building PyTorch wheel
RUN pip install --no-cache-dir wheel && \
    pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" && \
    pip install --no-cache-dir h5py && \
    pip install --no-cache-dir Cython && \
    pip install --no-cache-dir numpy scipy && \
    pip install --no-cache-dir matplotlib bokeh && \
    pip install --no-cache-dir pyproj && \
    pip install --no-cache-dir Cartopy && \
    pip install --no-cache-dir torchvision && \
    pip install --no-cache-dir keras && \
    pip install --no-cache-dir ipython && \
    pip install --no-cache-dir pandas && \
    pip install --no-cache-dir seaborn && \
    pip install --no-cache-dir pillow && \
    pip install --no-cache-dir networkx cvxpy && \
    pip install --no-cache-dir scikit-learn scikit-image && \
    pip install --no-cache-dir scikit-image && \
    pip install --no-cache-dir pygments && \
    pip install --no-cache-dir ipython && \
    pip install --no-cache-dir jupyter && \
    pip install --no-cache-dir jupyterlab && \
    rm -f /tmp/*.whl

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "PyTorch 0.2 on Python 3.6 (CUDA 8.0)" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Install PyTorch
RUN pip install --no-cache-dir \
        https://download.pytorch.org/whl/cu80/torch-0.2.0.post3-cp36-cp36m-manylinux1_x86_64.whl && \
    rm -rf /root/.cache

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

# vim: ft=dockerfile sts=4 sw=4 et tw=0
