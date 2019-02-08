FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/common-bazel:0.15-ubuntu as bazel-binary

# Build target: lablup/common-tensorflow:1.3-py36-gpu
FROM nvidia/cuda:9.0-base-ubuntu16.04
LABEL maintainer="Mario Cho <m.cho@lablup.com>"

# The TensorFlow version
ENV TF_VERSION 1.11
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
ENV TF_NEED_TENSORRT 1
ENV TF_NEED_MPI 0
#        TF_NEED_GDR=0 \
#        TF_NEED_S3=1 \
#        TF_NEED_KAFKA=0 \
#        TF_SET_ANDROID_WORKSPACE=0 \
ENV TF_CUDA_VERSION 9.0 
ENV TF_CUDNN_VERSION 7
ENV TF_NCCL_VERSION 2
ENV TF_CUDA_COMPUTE_CAPABILITIES 3.5,3.7,5.2,6.0,6.1,7.0
ENV LD_LIBRARY_PATH /usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH
ENV CUDA_TOOLKIT_PATH /usr/local/cuda
ENV CUDNN_INSTALL_PATH /usr/local/cuda 
#ENV NCCL_INSTALL_PATH /usr/lib/nccl 
#	MPI_HOME=/usr/lib/openmpi \
#	ANDROID_SDK_HOME=/root/Android \
#	ANDROID_NDK_HOME=/root/Android/android-ndk \

# Install system package dependencies
# NOTE: running bazel requires JDK, not JRE!
RUN apt-get update && \
    apt-get install -y --no-install-recommends openjdk-8-jdk-headless && \
    apt-get install -y --no-install-recommends \
        cuda-command-line-tools-9-0 \
        cuda-cublas-dev-9-0 \
        cuda-cudart-dev-9-0 \
        cuda-cufft-dev-9-0 \
        cuda-curand-dev-9-0 \
        cuda-cusolver-dev-9-0 \
        cuda-cusparse-dev-9-0 \
        curl \
        git \
        libcudnn7=7.2.1.38-1+cuda9.0 \
        libcudnn7-dev=7.2.1.38-1+cuda9.0 \
        libnccl2=2.2.13-1+cuda9.0 \
        libnccl-dev=2.2.13-1+cuda9.0 \
        libcurl3-dev \
        libfreetype6-dev \
        libhdf5-serial-dev \
        libpng12-dev \
        libzmq3-dev \
        pkg-config \
        rsync \
        software-properties-common \
        zip unzip \
        zlib1g-dev \
        wget \
        imagemagick \
        graphviz \
        cmake \
	gcc g++ \
        curl \
	wget \
	pkg-config \
        rsync \
        sed \
        swig \
        git-core \
	zip && \
    rm -rf /var/lib/apt/lists/* && \
    find /usr/local/cuda-9.0/lib64/ -type f -name 'lib*_static.a' -not -name 'libcudart_static.a' -delete && \
    rm /usr/lib/x86_64-linux-gnu/libcudnn_static_v7.a

RUN apt-get update && \
    apt-get install nvinfer-runtime-trt-repo-ubuntu1604-4.0.1-ga-cuda9.0 && \
    apt-get update && \
    apt-get install libnvinfer4=4.1.2-1+cuda9.0 && \
    apt-get install libnvinfer-dev=4.1.2-1+cuda9.0

# Link NCCL libray and header where the build script expects them.
RUN mkdir /usr/local/cuda-9.0/lib &&  \
    ln -s /usr/lib/x86_64-linux-gnu/libnccl.so.2 /usr/local/cuda/lib/libnccl.so.2 && \
    ln -s /usr/include/nccl.h /usr/local/cuda/include/nccl.h
    
COPY --from=bazel-binary /usr/local/bin/bazel /usr/local/bin/
COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

# Check Bazel/Python is runnable.
# Also install TensorFlow build dependencies (ensure we have proper numpy)
RUN bazel version; python -c "import sys; print(sys.prefix); print(sys.version_info)" && \
    pip install --no-cache-dir wheel numpy scipy && \
    pip install --no-cache-dir keras_applications && \
    pip install --no-cache-dir keras_preprocessing && \
    rm -f /tmp/*.whl


# NOTE: python should be linked to python3
RUN : build TensorFlow pip package \
    && cd /tmp \
    && curl -SL https://github.com/tensorflow/tensorflow/archive/r${TF_VERSION}.tar.gz | tar xzf - \ 
    && ldconfig
    
RUN cd /tmp/tensorflow-r${TF_VERSION} && \
    ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 && \
        LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} \
        tensorflow/tools/ci_build/builds/configured GPU \
#        CI_BUILD_PYTHON=python \
    bazel build -c opt --copt=-mavx --config=cuda \
	--cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
        tensorflow/tools/pip_package:build_pip_package && \
    rm /usr/local/cuda/lib64/stubs/libcuda.so.1 && \
    bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg && \
    rm -fr /root/.cache

RUN ls -lh /tmp/tensorflow_pkg

# vim: ft=dockerfile sts=4 sw=4 et tw=0


