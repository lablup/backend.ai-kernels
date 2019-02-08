FROM lablup/kernel-base:python3.6 as python-binary

FROM ubuntu:16.04 as base_build
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt-get update && apt-get install -y --no-install-recommends \
        automake \
        build-essential \
        ca-certificates \
        curl \
        git \
        libcurl3-dev \
        libfreetype6-dev \
        libpng12-dev \
        libtool \
        libzmq3-dev \
        mlocate \
        openjdk-8-jdk\
        openjdk-8-jre-headless \
        pkg-config \
        python-dev \
        software-properties-common \
        swig \
        zip unzip \
        wget \
        zip \
        zlib1g-dev \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY --from=python-binary /python.tar.gz /
RUN cd /; \
    tar xzpf python.tar.gz; \
    ldconfig; \
    rm -f python.tar.gz

# Prepare for building TensorFlwo wheel
RUN pip install --no-cache-dir wheel ; \
    pip install --no-cache-dir grpcio h5py ; \
    pip install --no-cache-dir keras_applications ; \
    pip install --no-cache-dir keras_preprocessing ; \
    pip install --no-cache-dir mock ; \
    pip install --no-cache-dir numpy ; \
    pip install --no-cache-dir requests ; \
    rm -f /tmp/*.whl

# Set up Bazel
# Need >= 0.15.0 so bazel compiles work with docker bind mounts.
ENV BAZEL_VERSION 0.15.0
WORKDIR /
RUN mkdir /bazel && \
    cd /bazel && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE && \
    chmod +x bazel-*.sh && \
    ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    cd / && \
    rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh

# Download TF Serving sources (optionally at specific commit).
WORKDIR /tensorflow-serving
RUN git clone --branch=master https://github.com/tensorflow/serving . && \
    git remote add upstream https://github.com/tensorflow/serving.git && \
    if [ head != "head" ]; then git checkout head; fi

FROM base_build as binary_build
# Build, and install TensorFlow Serving
ARG TF_SERVING_BUILD_OPTIONS="--config=nativeopt"
RUN echo "Building with build options: ${TF_SERVING_BUILD_OPTIONS}"
ARG TF_SERVING_BAZEL_OPTIONS=""
RUN echo "Building with Bazel options: ${TF_SERVING_BAZEL_OPTIONS}"

RUN CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" \
    bazel build --color=yes --curses=yes \
        --verbose_failures \
        --output_filter=DONT_MATCH_ANYTHING \
        --config=nativeopt \
        tensorflow_serving/model_servers:tensorflow_model_server && \
    cp bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server \
        /usr/local/bin/

# Build and install TensorFlow Serving API
RUN bazel build --color=yes --curses=yes \
        --verbose_failures \
        --output_filter=DONT_MATCH_ANYTHING \
        --config=nativeopt \
        tensorflow_serving/tools/pip_package:build_pip_package && \
    bazel-bin/tensorflow_serving/tools/pip_package/build_pip_package \
        /tmp/pip && \
    pip --no-cache-dir install --upgrade /tmp/pip/tensorflow_serving*.whl 

FROM binary_build as clean_build
# Clean up Bazel cache when done.
RUN bazel clean --expunge --color=yes && \
    rm -rf /root/.cache
CMD ["/bin/bash"]