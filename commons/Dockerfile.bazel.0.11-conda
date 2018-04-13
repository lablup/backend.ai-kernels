FROM bitnami/minideb:jessie

RUN export DEBIAN_FRONTEND=noninteractive && \
    echo "deb http://http.debian.net/debian jessie-backports main" | \
	tee --append /etc/apt/sources.list.d/jessie-backports.list > /dev/null && \
    apt-get update && \
    apt-get install -y --no-install-recommends -t jessie-backports openjdk-8-jdk-headless && \
    install_packages \
        build-essential \
        curl \
        python \
        git-core \
        unzip zip

ENV BAZEL_VERSION 0.11.1

# install Bazel to build TensorFlow
RUN curl -SLO https://github.com/bazelbuild/bazel/releases/download/${BAZEL_VERSION}/bazel-${BAZEL_VERSION}-dist.zip \
    && mkdir bazel-${BAZEL_VERSION} \
    && unzip -qd bazel-${BAZEL_VERSION} bazel-${BAZEL_VERSION}-dist.zip \
    && cd bazel-${BAZEL_VERSION} \
    && bash compile.sh \
    && cp -p output/bazel /usr/local/bin/

RUN ls -l /usr/local/bin/bazel*

# vim: ft=dockerfile
