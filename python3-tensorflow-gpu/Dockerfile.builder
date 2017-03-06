FROM anapsix/alpine-java:8_jdk

RUN apk add --no-cache python3 python3-dev build-base libc-dev curl libressl-dev swig
RUN cd tmp/ \
    && git clone https://github.com/tensorflow/tensorflow \
    && cd tensorflow \
    && git checkout r1.0

# TODO: install/build Bazel (BLOCKER NOW)

# TODO: copy detect-cuda.py and its dependencies

# TODO: run bazel build and extract the resulting wheel package.

# vim: sts=4 sw=4 et ft=dockerfile
