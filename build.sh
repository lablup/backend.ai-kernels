#! /bin/bash

TAG=$1
if [ -z "$TAG" ]; then
  echo "Please specify the target build tag (Linux distro type)"
  exit 1
fi
case "$TAG" in
  latest) ;;
  debian) ;;
  *)
    echo "Tag must be either latest or debian."
    exit 1
    ;;
esac
echo "Building for the target tag: $TAG"


build_kernel() {
  [[ "$TAG" = latest ]] && suffix="" || suffix=".debian"
  docker build -t lablup/kernel-$1:$TAG $2 -f $1/Dockerfile$suffix $1
}

build_squashed_kernel() {
  [[ "$TAG" = latest ]] && suffix="" || suffix=".debian"
  docker build -t lablup/kernel-$1:$TAG $2 -f $1/Dockerfile$suffix --squash $1
}

build_common() {
  [[ "$TAG" = latest ]] && suffix="-alpine" || suffix="-debian"
  docker build -t lablup/common-$1:$TAG $2 -f commons/Dockerfile.$1$suffix commons
}

tag_debian_as_latest() {
  docker tag lablup/kernel-$1:debian lablup/kernel-$1:latest
}


# dual stack
#build_kernel "base"
#build_squashed_kernel "base-python3-minimal"
#build_squashed_kernel "python3"
#build_common "bazel"

# alpine stack
if [ "$TAG" = latest ]; then
  echo "pass"
  #build_squashed_kernel "base-python2-minimal"
  #build_squashed_kernel "python2"
  #build_squashed_kernel "git"
  #build_kernel "c"
  #build_kernel "cpp"
  #build_kernel "java8"
  #build_kernel "rust"
  #build_squashed_kernel "julia" "--build-arg JULIA_VERSION=0.6.0"
fi


# debian stack
if [ "$TAG" = debian ]; then

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

  #docker build -t lablup/common-cuda:debian \
  #  --build-arg CUDA_FULL=$CUDA_FULL \
  #  --build-arg CUDA_MAJOR=$CUDA_MAJOR \
  #  --build-arg CUDA_MINOR=$CUDA_MINOR \
  #  --build-arg CUDA_MAJMIN=$CUDA_MAJMIN \
  #  --build-arg CUDNN_FULL=$CUDNN_FULL \
  #  --build-arg CUDNN_MAJOR=$CUDNN_MAJOR \
  #  --build-arg CUDNN_MINOR=$CUDNN_MINOR \
  #  --build-arg CUDNN_MAJMIN=$CUDNN_MAJMIN \
  #  -f Dockerfile.cuda-debian commons

  #docker build -t lablup/common-py3-tensorflow-cpu:1.3-debian --build-arg TF_VERSION=1.3.0 -f Dockerfile.tensorflow-py3-cpu commons
  #docker build -t lablup/common-py3-tensorflow-gpu:1.3-debian --build-arg TF_VERSION=1.3.0 -f Dockerfile.tensorflow-py3-gpu commons
  #docker build -t lablup/common-py3-tensorflow-cpu:1.2-debian --build-arg TF_VERSION=1.2.0 -f Dockerfile.tensorflow-py3-cpu commons
  #docker build -t lablup/common-py3-tensorflow-gpu:1.2-debian --build-arg TF_VERSION=1.2.0 -f Dockerfile.tensorflow-py3-gpu commons
  #docker build -t lablup/common-py3-tensorflow-cpu:1.1-debian --build-arg TF_VERSION=1.1.0 -f Dockerfile.tensorflow-py3-cpu commons
  #docker build -t lablup/common-py3-tensorflow-gpu:1.1-debian --build-arg TF_VERSION=1.1.0 -f Dockerfile.tensorflow-py3-gpu commons

  #build_squashed_kernel "python3-tensorflow"
  #build_squashed_kernel "python3-tensorflow-gpu"
  build_squashed_kernel "python3-torch"
  build_squashed_kernel "python3-torch-gpu"

  #tag_debian_as_latest "python3-tensorflow"
  #tag_debian_as_latest "python3-tensorflow-gpu"
  tag_debian_as_latest "python3-torch"
  tag_debian_as_latest "python3-torch-gpu"

fi
