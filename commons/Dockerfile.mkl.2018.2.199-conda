FROM lablup/common-bazel:0.11-debian as bazel-binary

# Build target: lablup/common-tensorflow:1.7-py36-mkl
# ref: https://github.com/tatsushid/docker-alpine-py3-tensorflow-jupyter/blob/master/Dockerfile
FROM lablup/kernel-python:3.6-conda

COPY --from=bazel-binary /usr/local/bin/bazel /usr/local/bin/

RUN install_packages libcups2 && \
    echo "deb http://http.debian.net/debian jessie-backports main" | \
        tee --append /etc/apt/sources.list.d/jessie-backports.list > /dev/null && \
    apt-get update && \
    apt-get install -y --no-install-recommends -t jessie-backports openjdk-8-jdk-headless && \
    install_packages \
        imagemagick \
        graphviz \
        cmake \
        curl \
        build-essential \
        perl \
        rsync \
        sed \
        swig \
        git-core \
        unzip zip

# Check Bazel is runnable.
RUN bazel version

ENV MKL_VERSION 2018.2.199

# Install MKL
RUN cd /tmp && \
  wget http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/12725/l_mkl_${MKL_VERSION}.tgz \
  && tar -xzf l_mkl_${MKL_VERSION}.tgz && \
  cd l_mkl_${MKL_VERSION} && \
  sed -i 's/ACCEPT_EULA=decline/ACCEPT_EULA=accept/g' silent.cfg && \
  ./install.sh -s silent.cfg && \
  cd .. && \
  rm -rf *

RUN echo "/opt/intel/mkl/lib/intel64" >> /etc/ld.so.conf.d/intel.conf && \
  ldconfig && \
  echo ". /opt/intel/bin/compilervars.sh intel64" >> /etc/bash.bashrc

# Install numpy with MKL
RUN pip install Cython

RUN cd /tmp && \
  git clone https://github.com/numpy/numpy.git numpy && \
  cd numpy && \
  cp site.cfg.example site.cfg && \
  echo "\n[mkl]" >> site.cfg && \
  echo "include_dirs = /opt/intel/mkl/include/intel64/" >> site.cfg && \
  echo "library_dirs = /opt/intel/mkl/lib/intel64/" >> site.cfg && \
  echo "mkl_libs = mkl_rt" >> site.cfg && \
  echo "lapack_libs =" >> site.cfg && \
  python setup.py build --fcompiler=gnu95 && \
  python setup.py install

# Install scipy
RUN cd /tmp && \
  git clone https://github.com/scipy/scipy.git scipy && \
  cd scipy && \
  python setup.py build && \
  python setup.py install

# Limit the system resource used during compilation
ENV LOCAL_RESOURCES 4096,8.0,1.0

# The TensorFlow version
ENV TF_VERSION 1.7

RUN : build TensorFlow pip package \
    && cd /tmp \
    && curl -SL https://github.com/tensorflow/tensorflow/archive/r${TF_VERSION}.tar.gz \
        | tar xzf -
RUN cd /tmp/tensorflow-r${TF_VERSION} \
    # temporary fix for tensorflow/tensorflow#12979
    && sed -i '\@https://github.com/google/protobuf/archive/0b059a3d8a8f8aa40dde7bea55edca4ec5dfea66.tar.gz@d' tensorflow/workspace.bzl \
    && PYTHON_BIN_PATH=/usr/local/bin/python \
        PYTHON_LIB_PATH=/usr/local/lib/python3.6/site-packages \
        GCC_HOST_COMPILER_PATH=/usr/bin/gcc \
        CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" \
        ENV CI_BUILD_PYTHON=python \
        LD_LIBRARY_PATH=${LD_LIBRARY_PATH} \
        PYTHON_BIN_PATH=/usr/bin/python \
        PYTHON_LIB_PATH=/usr/local/lib/python3.6/dist-packages \
        CC_OPT_FLAGS='-march=native' \
        TF_NEED_JEMALLOC=1 \
        TF_NEED_S3=0 \
        TF_NEED_GDR=0 \
        TF_NEED_VERBS=0 \
        TF_NEED_MKL=1 \
        TF_NEED_JEMALLOC=1 \
        TF_NEED_GCP=0 \
        TF_NEED_HDFS=0 \
        TF_ENABLE_XLA=0 \
        TF_NEED_VERBS=0 \
        TF_NEED_OPENCL=0 \
        TF_NEED_CUDA=0 \
        TF_NEED_MPI=0 \
        bash configure \
    && bazel build \
        --config=opt --local_resources ${LOCAL_RESOURCES} \
        --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
        //tensorflow/tools/pip_package:build_pip_package \
    && ./bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg

RUN ls -l /tmp/tensorflow_pkg

# vim: ft=dockerfile
