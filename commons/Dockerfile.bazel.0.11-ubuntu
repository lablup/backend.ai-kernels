FROM lablup/kernel-base-python-minimal:3.6-ubuntu

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends openjdk-8-jdk-headless && \
    apt-get install -y \
        build-essential \
        curl \
        python \
        git-core \
        unzip zip

ENV BAZEL_VERSION 0.11.0

# install Bazel to build TensorFlow
RUN curl -SLO https://github.com/bazelbuild/bazel/releases/download/${BAZEL_VERSION}/bazel-${BAZEL_VERSION}-dist.zip && \
    mkdir bazel-${BAZEL_VERSION} && \
    unzip -qd bazel-${BAZEL_VERSION} bazel-${BAZEL_VERSION}-dist.zip && \
    cd bazel-${BAZEL_VERSION} && \
    bash compile.sh && \
    cp -p output/bazel /usr/local/bin/ && \
    ls -l /usr/local/bin/bazel*

# vim: ft=dockerfile
