#!/bin/bash

# You need to run this in the host, not inside containers!

set -o nounset
set -o errexit

BUILD_DIR=./tensorflow_build

# Prepare TensorFlow source
# NOTE: maybe skipped already done, just pull latest version)
git clone https://github.com/tensorflow/tensorflow $BUILD_DIR


# Prepare our version detector
python _cuda_query.py
cp _cuda_query.*.so $BUILD_DIR
cp detect-cuda.py $BUILD_DIR


# Original dependencies:
#   - openjdk-8-jdk libcurl4-openssl-dev swig
#   - python3-numpy python3-dev python3-pip python3-setuptools (not required for us)
# NOTE: maybe skipped already done
apt-get install -y openjdk-8-jdk libcurl4-openssl-dev swig


# Install Bazel (Google's build toolchain)
# NOTE: maybe skipped already done
echo "deb http://storage.googleapis.com/bazel-apt stable jdk1.8" | tee /etc/apt/sources.list.d/bazel.list
curl https://storage.googleapis.com/bazel-apt/doc/apt-key.pub.gpg | apt-key add -
apt-get update && apt-get install -y bazel


# Build TensorFlow fitted for our system
cd $BUILD_DIR

PYTHON_BIN_PATH=`which python` \
TF_NEED_GCP=0 \
TF_NEED_HDFS=0 \
TF_NEED_CUDA=1 \
GCC_HOST_COMPILER_PATH=/usr/bin/gcc \
TF_CUDA_VERSION=`python detect-cuda.py --cuda-ver` CUDA_TOOLKIT_PATH=/usr/local/cuda \
TF_CUDNN_VERSION=`python detect-cuda.py --cudnn-ver` CUDNN_INSTALL_PATH=/usr/local/cuda \
TF_CUDA_COMPUTE_CAPABILITIES=`python detect-cuda.py --compute-caps` ./configure

bazel build -c opt --config=cuda //tensorflow/tools/pip_package:build_pip_package
bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg


# Install the compiled wheel
pip install /tmp/tensorflow_pkg/tensorflow-*.whl
