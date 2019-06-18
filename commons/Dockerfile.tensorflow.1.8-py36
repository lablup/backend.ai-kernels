# Build target: lablup/common-tensorflow:1.8-py36

# import python-binary
FROM lablup/kernel-base:python3.6 as python-binary

FROM ubuntu:16.04
LABEL maintainer="Mario Cho <m.cho@lablup.com>"

# The TensorFlow version
ENV TF_VERSION 1.8
ENV BAZEL_VERSION 0.11.0
ENV PYTHON_BIN_PATH /usr/local/bin/python
ENV PYTHON_LIB_PATH /usr/local/lib/python3.6/site-packages 
ENV GCC_HOST_COMPILER_PATH /usr/bin/gcc 
ENV CC_OPT_FLAGS "-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" 
ENV TF_NEED_JEMALLOC 1
ENV TF_NEED_GCP 1
ENV TF_NEED_HDFS 1
ENV TF_ENABLE_XLA 1
ENV TF_NEED_VERBS 0
ENV TF_NEED_MPI 0
ENV TF_NEED_GDR 1
ENV TF_NEED_S3 1 
ENV TF_NEED_KAFKA 0 
ENV TF_NEED_OPENCL 0
ENV TF_NEED_OPENCL_SYCL 0
ENV TF_NEED_CUDA 0
ENV TF_CUDA_CLANG 0
ENV TF_DOWNLOAD_CLANG 0
ENV TF_NEED_TENSORRT 0
ENV TF_SET_ANDROID_WORKSPACE 0

# Install system package dependencies
# NOTE: running bazel requires JDK, not JRE!
RUN apt-get update && \
    apt-get install -y --no-install-recommends openjdk-8-jdk-headless && \
    apt-get install -y --no-install-recommends \
    	gcc g++ make \
	libssl-dev \
        libfreetype6-dev \
        libhdf5-serial-dev \
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
        curl \
	wget \
        rsync \
        sed \
        swig \
        git-core \
	libcurl3-dev \
	zip && \
    rm -rf /var/lib/apt/lists/*

COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

# Prepare for building TensorFlow wheel
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools pip && \
    python3 -m pip install --no-cache-dir \
    	wheel \
    	numpy \
	scipy \
        sklearn \
        pandas \
        keras_applications \
        keras_preprocessing \
        matplotlib \
	future \
	&& \
    rm -f /tmp/*.whl

# Check Python is runnable.
RUN python3 -c "import sys; print(sys.prefix); print(sys.version_info)"

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
    python3 -c "import sys; print(sys.prefix); print(sys.version_info)" && \
    python3 -c "import numpy; numpy.show_config()" && \
    python3 -c "import scipy; scipy.show_config()" 
    
RUN : build TensorFlow pip package && \
    cd /tmp && \
    git clone --branch=r${TF_VERSION} --depth=1 https://github.com/tensorflow/tensorflow.git tensorflow-${TF_VERSION} && \
    ldconfig

RUN cd /tmp/tensorflow-${TF_VERSION} && \
# AVX & AVX2 support under haswell core
        CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" \
# AVX512 support over skylake core
#        CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mavx512f -mavx512pf -mavx512cd -mavx512er -mfpmath=both -msse4.1 -msse4.2" \
    tensorflow/tools/ci_build/builds/configured CPU \
    bazel build \
    	-c opt \
	--copt=-mavx \
	--copt=-mavx2 \
	--config=mkl \
	--config=monolithic \
#	--config=gdr \
#	--config=verbs \
#	--config=ngraph \
        --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
        //tensorflow/tools/pip_package:build_pip_package && \
    ./bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg && \
    rm -fr /root/.cache
    
RUN ls -l /tmp/tensorflow_pkg

# vim: ft=dockerfile sts=4 sw=4 et tw=0