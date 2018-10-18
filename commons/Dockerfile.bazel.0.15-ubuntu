FROM lablup/kernel-base-python-minimal:3.6-ubuntu

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends openjdk-8-jdk-headless && \
    apt-get install -y \
        build-essential \
        curl \
        python \
        git-core \
        unzip zip


# setup the Bazel 
ENV BAZEL_VERSION 0.15.0

# Running bazel inside a `docker build` command causes trouble, cf:
#   https://github.com/bazelbuild/bazel/issues/134
# The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >>/etc/bazel.bazelrc
#   https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" \
    >> /etc/bazel.bazelrc

# install the Bazel to build TensorFlow
RUN curl -SLO https://github.com/bazelbuild/bazel/releases/download/${BAZEL_VERSION}/bazel-${BAZEL_VERSION}-dist.zip && \
    mkdir bazel-${BAZEL_VERSION} && \
    unzip -qd bazel-${BAZEL_VERSION} bazel-${BAZEL_VERSION}-dist.zip && \
    cd bazel-${BAZEL_VERSION} && \
    bash compile.sh && \
    cp -p output/bazel /usr/local/bin/ && \
    ls -l /usr/local/bin/bazel* 

# vim: ft=dockerfile