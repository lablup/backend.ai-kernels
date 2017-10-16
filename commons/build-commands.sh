#!/bin/sh

# Not for actual execution -- this is the reference for build commands!


#docker build -t lablup/common-glibc:alpine -f Dockerfile.glibc-alpine .
#docker build -t lablup/common-bazel:alpine -f Dockerfile.bazel-alpine .
#docker build -t lablup/common-cuda:alpine -f Dockerfile.cuda-alpine .


docker build -t lablup/common-bazel:debian -f Dockerfile.bazel-debian .

CUDA_VERSION="8.0.61_375.26"
CUDNN_VERSION="6.0"

CUDA_FULL="$CUDA_VERSION"
CUDA_MAJOR="$(echo $CUDA_VERSION | cut -d. -f1)"
CUDA_MINOR="$(echo $CUDA_VERSION | cut -d. -f2)"
CUDA_MAJMIN="$CUDA_MAJOR.$CUDA_MINOR"
CUDNN_FULL="$CUDNN_VERSION"
CUDNN_MAJOR="$(echo $CUDNN_VERSION | cut -d. -f1)"
CUDNN_MINOR="$(echo $CUDNN_VERSION | cut -d. -f2)"
CUDNN_MAJMIN="$CUDNN_MAJOR.$CUDNN_MINOR"

docker build -t lablup/common-cuda:debian \
  --build-arg CUDA_FULL=$CUDA_FULL \
  --build-arg CUDA_MAJOR=$CUDA_MAJOR \
  --build-arg CUDA_MINOR=$CUDA_MINOR \
  --build-arg CUDA_MAJMIN=$CUDA_MAJMIN \
  --build-arg CUDNN_FULL=$CUDNN_FULL \
  --build-arg CUDNN_MAJOR=$CUDNN_MAJOR \
  --build-arg CUDNN_MINOR=$CUDNN_MINOR \
  --build-arg CUDNN_MAJMIN=$CUDNN_MAJMIN \
  -f Dockerfile.cuda-debian .

docker build -t lablup/common-py3-tensorflow-cpu:1.3-debian --build-arg TF_VERSION=1.3.0 -f Dockerfile.tensorflow-py3-cpu .
docker build -t lablup/common-py3-tensorflow-gpu:1.3-debian --build-arg TF_VERSION=1.3.0 -f Dockerfile.tensorflow-py3-gpu .

#docker build -t lablup/common-py3-tensorflow-cpu:1.2-debian --build-arg TF_VERSION=1.2.0 -f Dockerfile.tensorflow-py3-cpu .
#docker build -t lablup/common-py3-tensorflow-gpu:1.2-debian --build-arg TF_VERSION=1.2.0 -f Dockerfile.tensorflow-py3-gpu .
#
#docker build -t lablup/common-py3-tensorflow-cpu:1.1-debian --build-arg TF_VERSION=1.1.0 -f Dockerfile.tensorflow-py3-cpu .
#docker build -t lablup/common-py3-tensorflow-gpu:1.1-debian --build-arg TF_VERSION=1.1.0 -f Dockerfile.tensorflow-py3-gpu .

# vim: ft=sh
