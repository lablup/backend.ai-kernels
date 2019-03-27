FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/common-tensorflow:1.12-py36-srv as tf-serving

FROM ubuntu:18.04
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt update -y && \
    apt install -y \
        ca-certificates \
        wget curl git-core \
        python3 python3-pip \	
        libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g \
	libmpdec2 \
	libssl-dev \
        libncursesw5 libtinfo5 libreadline6 \
	proj-bin \
        libgeos-dev \
        mime-support \
	gcc g++ \
        libproj-dev libgeos-dev \	
        libzmq3-dev libuv1

RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH && \
    python -c 'import sys; print(sys.version_info); import ssl'

# Install TensorFlow build dependencies (ensure we have proper numpy)
COPY --from=tf-serving /usr/local/bin/tensorflow_model_server /usr/bin/tensorflow_model_server
COPY --from=tf-serving /tmp/pip/tensorflow_serving*.whl /tmp/

# Prepare for building TensorFlwo wheel
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir wheel && \
    python3 -m pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    python3 -m pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir pyproj && \
    python3 -m pip install --no-cache-dir Cartopy && \
    python3 -m pip install --no-cache-dir wheel /tmp/*.whl && \
    python3 -m pip install --no-cache-dir keras && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir pandas && \
    python3 -m pip install --no-cache-dir seaborn && \
    python3 -m pip install --no-cache-dir pillow && \
    python3 -m pip install --no-cache-dir networkx cvxpy && \
    python3 -m pip install --no-cache-dir scikit-learn scikit-image && \
    python3 -m pip install --no-cache-dir pygments && \
    python3 -m pip install --no-cache-dir requests && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# python alternative support
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "TensorFlow 1.12 Serving on Python 3.6 (CPU-only)" && \
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
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090,gRPC:pty:8500,REST:http:8501"

COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile
