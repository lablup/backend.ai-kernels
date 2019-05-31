# Lablup Next Generation 19.05
# Tensorflow 1.14 (CUDA9) & Python 3.6
# ref: https://tensorflow.org

# import python-binary
FROM lablup/kernel-base:python3.6 as python-binary

# Build target: lablup/common-tensorflow:1.14-py36-cuda9
FROM nvidia/cuda:9.0-base-ubuntu16.04
LABEL maintainer="Mario Cho <m.cho@lablup.com>"

# The TensorFlow version
ENV TF_VERSION 1.14
ENV BAZEL_VERSION 0.24.1
ENV CUDA_VERSION 9.0.176
ENV CUDNN_VERSION 7.4.2.24
ENV NCCL_VERSION 2.4.2
ENV TENSORT_VERSION 5.0.2
ENV PYTHON_BIN_PATH /usr/local/bin/python
ENV PYTHON_LIB_PATH /usr/local/lib/python3.6/site-packages 
ENV GCC_HOST_COMPILER_PATH /usr/bin/gcc 
ENV CC_OPT_FLAGS "-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" 
ENV TF_NEED_JEMALLOC 1
ENV TF_NEED_GCP 1
ENV TF_NEED_HDFS 1
ENV TF_ENABLE_XLA 1
ENV TF_NEED_VERBS 0
ENV TF_NEED_OPENCL_SYCL 0
ENV TF_NEED_CUDA 1
ENV TF_CUDA_CLANG 0
ENV TF_NEED_TENSORRT 0
ENV TF_NEED_MPI 0
ENV TF_NEED_S3 1
ENV TF_CUDA_VERSION 9.0 
ENV TF_CUDNN_VERSION 7
ENV TF_NCCL_VERSION 2
ENV TF_CUDA_COMPUTE_CAPABILITIES 3.5,3.7,5.2,6.0,6.1,7.0 
ENV LD_LIBRARY_PATH /usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH
ENV CUDA_TOOLKIT_PATH /usr/local/cuda
#ENV CUDNN_INSTALL_PATH /usr/local/cuda 
ENV CI_BUILD_PYTHON python

# Install system package dependencies
# NOTE: running bazel requires JDK, not JRE!
RUN apt-get update && \
    apt-get install nvinfer-runtime-trt-repo-ubuntu1604-$TENSORT_VERSION-ga-cuda9.0 && \
    apt-get update && \     
    apt-get install -y --no-install-recommends openjdk-8-jdk-headless && \
    apt-get install -y --no-install-recommends \
        curl gcc g++ make cmake git \
        cuda-command-line-tools-9-0 \
        cuda-cublas-dev-9-0 \
        cuda-cudart-dev-9-0 \
        cuda-cufft-dev-9-0 \
        cuda-curand-dev-9-0 \
        cuda-cusolver-dev-9-0 \
        cuda-cusparse-dev-9-0 \
        libcudnn7=$CUDNN_VERSION-1+cuda9.0 \
        libcudnn7-dev=$CUDNN_VERSION-1+cuda9.0 \
	libnvinfer5=$TENSORT_VERSION-1+cuda9.0 \
	libnvinfer-dev=$TENSORT_VERSION-1+cuda9.0 \
        libnccl2=$NCCL_VERSION-1+cuda9.0 \
        libnccl-dev=$NCCL_VERSION-1+cuda9.0 \
	libssh-dev \
	proj-bin libproj-dev \
	libgeos-dev libgeos++-dev \
        libcurl3-dev \
        libfreetype6-dev \
        libhdf5-serial-dev \
        libpng12-dev \
        libzmq3-dev \
        libffi-dev \
        pkg-config \
        rsync \
        software-properties-common \
        zip unzip \
        zlib1g-dev \
        wget \
        imagemagick \
        graphviz \
        rsync \
        sed \
        swig \
        git-core \
	zip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    find /usr/local/cuda-9.0/lib64/ -type f -name 'lib*_static.a' -not -name 'libcudart_static.a' -delete && \
    rm /usr/lib/x86_64-linux-gnu/libcudnn_static_v7.a

# Link NCCL libray and header where the build script expects them.
RUN mkdir /usr/local/cuda-9.0/lib &&  \
    ln -s /usr/lib/x86_64-linux-gnu/libnccl.so.2 /usr/local/cuda/lib/libnccl.so.2 && \
    ln -s /usr/include/nccl.h /usr/local/cuda/include/nccl.h && \
    ln -s /usr/include/cuda-9.0/lib64/cudnn.h /etc/alternatives/cudnn.h 
    
COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

# Check Bazel/Python is runnable.
# Also install TensorFlow build dependencies (ensure we have proper numpy)
RUN python -c "import sys; print(sys.prefix); print(sys.version_info)" && \
    pip install --no-cache-dir wheel numpy scipy && \
    pip install --no-cache-dir keras_applications && \
    pip install --no-cache-dir keras_preprocessing && \
    pip install --no-cache-dir future && \
    rm -f /tmp/*.whl

# Running bazel inside a `docker build` command causes trouble, cf:
#   https://github.com/bazelbuild/bazel/issues/134
# The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >>/etc/bazel.bazelrc
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >>/etc/bazel.bazelrc

WORKDIR /
RUN mkdir /bazel && \
    cd /bazel && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE && \
    chmod +x bazel-*.sh && \
    ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    cd / && \
    rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    bazel version && \
    python -c "import sys; print(sys.prefix); print(sys.version_info)" && \
    python -c "import numpy; numpy.show_config()" && \
    python -c "import scipy; scipy.show_config()" 

RUN : build TensorFlow pip package && \
    cd /tmp && \
    git clone --branch=r${TF_VERSION} --depth=1 https://github.com/tensorflow/tensorflow.git tensorflow-${TF_VERSION} && \
    ldconfig
    
RUN cd /tmp/tensorflow-${TF_VERSION} && \
    ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 && \
        LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} \
        tensorflow/tools/ci_build/builds/configured GPU \
    bazel build \
        -c opt \
	--copt=-mavx \
	--copt=-mavx2 \
	--config=cuda \
	--cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
        tensorflow/tools/pip_package:build_pip_package && \
    rm /usr/local/cuda/lib64/stubs/libcuda.so.1 && \
    bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg && \
    rm -fr /root/.cache

RUN ls -lh /tmp/tensorflow_pkg

# vim: ft=dockerfile sts=4 sw=4 et tw=0