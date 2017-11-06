# Build target: lablup/common-cuda:debian
FROM bitnami/minideb:jessie

# NVIDIA package dependencies
RUN install_packages ca-certificates curl

ENV CUDA_FULL="8.0.61_375.26" \
    CUDA_MAJOR="8" \
    CUDA_MINOR="0" \
    CUDA_MAJMIN="8.0"

# Download and extract CUDA toolkit
RUN curl -Lo /root/cuda-all.run https://developer.nvidia.com/compute/cuda/${CUDA_MAJMIN}/Prod2/local_installers/cuda_${CUDA_FULL}_linux-run
RUN install_packages perl libncurses5 gcc g++
RUN export PERL5LIB=. && \
    sh /root/cuda-all.run --toolkit --silent --override && \
    ls -l /usr/local && \
    rm -f cuda-all.run

# Download and extract cuDNN library
# ref: https://gitlab.com/nvidia/cuda/blob/centos7/8.0/runtime/cudnn7/Dockerfile

ENV CUDNN_FULL="6.0" \
    CUDNN_MAJOR="6" \
    CUDNN_MINOR="0" \
    CUDNN_MAJMIN="6.0"

# TensorFlow 1.3 is built with cuDNN 6
# TensorFlow 1.4 will use cuDNN 7
RUN curl -Lo /root/cudnn.tgz \
    http://developer.download.nvidia.com/compute/redist/cudnn/v${CUDNN_MAJMIN}/cudnn-${CUDA_MAJMIN}-linux-x64-v${CUDNN_MAJMIN}.tgz
RUN tar --no-same-owner -xzf /root/cudnn.tgz -C /usr/local && \
    mkdir -p /etc/ld.so.conf.d && \
    echo "/usr/local/cuda/lib64" > /etc/ld.so.conf.d/cuda.conf && \
    echo "/usr/local/nvidia/lib64" > /etc/ld.so.conf.d/nvidia.conf && \
    ldconfig

# vim: ft=dockerfile
