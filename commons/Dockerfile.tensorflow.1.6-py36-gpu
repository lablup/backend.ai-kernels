FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/common-bazel:0.11-ubuntu as bazel-binary

# Build target: lablup/common-tensorflow:1.6-py36-gpu
FROM nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04

# Install system package dependencies
# NOTE: running bazel requires JDK, not JRE!
RUN apt-get update && \
    apt-get install -y --no-install-recommends openjdk-8-jdk-headless && \
    apt-get install -y --no-install-recommends \
        imagemagick \
        graphviz \
        cmake \
        curl \
        build-essential \
        perl \
	pkg-config \
        rsync \
        sed \
        swig \
        git-core \
        zip unzip \
	zip && \
     rm -rf /var/lib/apt/lists/* 

COPY --from=bazel-binary /usr/local/bin/bazel /usr/local/bin/
COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

# Check Bazel/Python is runnable.
# Also install TensorFlow build dependencies (ensure we have proper numpy)
RUN bazel version; python -c "import sys; print(sys.prefix); print(sys.version_info)" && \
    pip install --no-cache-dir wheel numpy

# The TensorFlow version
ENV TF_VERSION 1.6

# NOTE: python should be linked to python3
RUN : build TensorFlow pip package \
    && cd /tmp \
    && curl -SL https://github.com/tensorflow/tensorflow/archive/r${TF_VERSION}.tar.gz \
        | tar xzf -
RUN echo "/usr/local/cuda/lib64/stubs" > /etc/ld.so.conf.d/nvidia-stubs.conf && \
    LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} \
    ldconfig

RUN : build TensorFlow pip package && \
    cd /tmp && \
    curl -SL https://github.com/tensorflow/tensorflow/archive/r${TF_VERSION}.tar.gz \
        | tar xzf - && \
    cd /tmp/tensorflow-r${TF_VERSION} && \
        PYTHON_BIN_PATH=/usr/local/bin/python \
        PYTHON_LIB_PATH=/usr/local/lib/python3.6/site-packages \
        GCC_HOST_COMPILER_PATH=/usr/bin/gcc \
        CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" \
        TF_NEED_JEMALLOC=1 \
        TF_NEED_GCP=1 \
        TF_NEED_HDFS=1 \
        TF_ENABLE_XLA=1 \
        TF_NEED_VERBS=0 \
        TF_NEED_OPENCL=0 \
        TF_NEED_OPENCL_SYCL=0 \
        TF_NEED_CUDA=1 \
        TF_CUDA_CLANG=0 \
        TF_DOWNLOAD_CLANG=0 \
        TF_NEED_TENSORRT=0 \
        TF_NEED_MPI=0 \
        TF_NEED_GDR=0 \
        TF_NEED_S3=1 \
        TF_NEED_KAFKA=0 \
        TF_SET_ANDROID_WORKSPACE=0 \
        TF_CUDA_VERSION=9.0 \
        TF_CUDNN_VERSION=7 \
        TF_NCCL_VERSION=2 \
        TF_CUDA_COMPUTE_CAPABILITIES=3.5,3.7,5.2,6.0,6.1,7.0 \
        CUDA_TOOLKIT_PATH=/usr/local/cuda \
        CUDNN_INSTALL_PATH=/usr/local/cuda \
        NCCL_INSTALL_PATH=/usr \
    bash configure && \
    cd /tmp/tensorflow-r${TF_VERSION} ; \
    sed -r -i'' "s@http://www.sqlite.org@https://www.sqlite.org@" tensorflow/workspace.bzl ; \
    PYTHON_BIN_PATH=/usr/local/bin/python \
    PYTHON_LIB_PATH=/usr/local/lib/python3.6/site-packages \
    bazel fetch //tensorflow/tools/pip_package:build_pip_package

RUN cd /tmp/tensorflow-r${TF_VERSION}; \
    PYTHON_BIN_PATH=/usr/local/bin/python \
    PYTHON_LIB_PATH=/usr/local/lib/python3.6/site-packages \
    bazel build \
        --config=opt --config=cuda \
        --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
        //tensorflow/tools/pip_package:build_pip_package && \
    ./bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg && \
    rm /usr/local/cuda/lib64/stubs/libcuda.so.1 && \        
    rm -rf /root/.cache && \
    ls -lh /tmp/tensorflow_pkg

# vim: ft=dockerfile sts=4 sw=4 et tw=0


