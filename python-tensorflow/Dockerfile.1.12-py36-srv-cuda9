FROM lablup/kernel-base:jail as jail-builder
FROM lablup/kernel-base:hook as hook-builder
FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/common-tensorflow:1.12-py36-srv as tf-serving

FROM nvidia/cuda:9.0-base-ubuntu16.04
MAINTAINER Mario Cho "m.cho@lablup.com"

ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1
ENV NCCL_VERSION=2.2.13
ENV CUDNN_VERSION=7.2.1.38
ENV TF_TENSORRT_VERSION=4.1.2

ARG TF_SERVING_VERSION_GIT_BRANCH=master
ARG TF_SERVING_VERSION_GIT_COMMIT=head
LABEL tensorflow_serving_github_branchtag=${TF_SERVING_VERSION_GIT_BRANCH}
LABEL tensorflow_serving_github_commit=${TF_SERVING_VERSION_GIT_COMMIT}


RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        cuda-command-line-tools-9-0 \
        cuda-command-line-tools-9-0 \
        cuda-cublas-9-0 \
        cuda-cufft-9-0 \
        cuda-curand-9-0 \
        cuda-cusolver-9-0 \
        cuda-cusparse-9-0 \
        libcudnn7=${CUDNN_VERSION}-1+cuda9.0 \
        libnccl2=${NCCL_VERSION}-1+cuda9.0 \
        libgomp1 \
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
        libzmq3-dev libuv1 \
	&& \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install --no-install-recommends \
        nvinfer-runtime-trt-repo-ubuntu1604-4.0.1-ga-cuda9.0 && \
    apt-get update && \
    apt-get install --no-install-recommends \
        libnvinfer4=${TF_TENSORRT_VERSION}-1+cuda9.0 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm /usr/lib/x86_64-linux-gnu/libnvinfer_plugin* && \
    rm /usr/lib/x86_64-linux-gnu/libnvcaffe_parser* && \
    rm /usr/lib/x86_64-linux-gnu/libnvparsers*
					    

# Install TensorFlow-serving
COPY --from=tf-serving /usr/local/bin/tensorflow_model_server /usr/bin/tensorflow_model_server

# Expose ports
# gRPC
EXPOSE 8500

# REST
EXPOSE 8501


# Copy the whole Python from the docker library image
COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig
RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
# Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

# As we mostly have "manylinux" glibc-compatible binary packaes,
# we don't have to rebuild these!
RUN pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2"

# Install CUDA-9.0 + cuDNN 7.2
RUN ln -s /usr/local/cuda-9.0 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.2.1 /usr/local/cuda/lib64/libcudnn.so && \
    ldconfig
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

# python package install
RUN pip install --no-cache-dir wheel && \
    pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" && \
    pip install --no-cache-dir keras && \
    pip install --no-cache-dir h5py && \
    pip install --no-cache-dir Cython && \
    pip install --no-cache-dir matplotlib bokeh && \
    pip install --no-cache-dir pyproj && \
    pip install --no-cache-dir Cartopy && \
    pip install --no-cache-dir ipython && \
    pip install --no-cache-dir pandas && \
    pip install --no-cache-dir seaborn && \
    pip install --no-cache-dir pillow && \
    pip install --no-cache-dir networkx cvxpy && \
    pip install --no-cache-dir scikit-learn scikit-image && \
    pip install --no-cache-dir scikit-image && \
    pip install --no-cache-dir pygments && \
    pip install --no-cache-dir requests && \
    rm -f /tmp/*.whl

# Set where models should be stored in the container
#ENV MODEL_BASE_PATH=/home/work/models
#RUN mkdir -p ${MODEL_BASE_PATH}

# The only required piece is the model name in order to differentiate endpoints
#ENV MODEL_NAME=model

RUN apt-get update && \
    apt-get install -y libseccomp2 gosu && \
    apt-get clean && \
    rm -r /var/lib/apt/lists /var/cache/apt/archives && \
    ln -s /usr/sbin/gosu /usr/sbin/su-exec && \
    mkdir /home/work && chmod 755 /home/work; \
    mkdir /home/backend.ai && chmod 755 /home/backend.ai
ADD entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Create a script that runs the model server so we can use environment variables
# while also passing in arguments from the docker command line
#RUN echo '#!/bin/bash \n\n\
#    tensorflow_model_server --port=8500 --rest_api_port=8501 \
#        --model_name=${MODEL_NAME} --model_base_path=${MODEL_BASE_PATH}/${MODEL_NAME} \
#        "$@"' >> /usr/local/bin/entrypoint.sh && \
#    chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY policy.yml /home/backend.ai/policy.yml

# Install jail
COPY --from=jail-builder /go/src/github.com/lablup/backend.ai-jail/backend.ai-jail /home/backend.ai/jail
COPY --from=hook-builder /root/backend.ai-hook/libbaihook.so /home/backend.ai/libbaihook.so
ENV LD_PRELOAD /home/backend.ai/libbaihook.so

# Install kernel-runner scripts package
RUN pip install --no-cache-dir "backend.ai-kernel-runner[python]~=1.4.0"

# Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR /home/backend.ai/.matplotlib
RUN mkdir /home/backend.ai/.matplotlib
COPY matplotlibrc /home/backend.ai/.matplotlib/
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
    && python /tmp/matplotlib-fontcache.py \
    && rm /tmp/matplotlib-fontcache.py

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL ai.backend.nvidia.enabled="yes" \
      com.nvidia.cuda.version="9.0.176" \
      com.nvidia.volumes.needed="nvidia_driver" \
      ai.backend.port="8500, 8501" \
      ai.backend.timeout="0" \
      ai.backend.maxmem="8g" \
      ai.backend.maxcores="4" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input"


CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "python"]

# vim: ft=dockerfile sts=4 sw=4 et tw=0