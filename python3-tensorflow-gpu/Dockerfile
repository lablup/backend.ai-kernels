# NOTICE: This file is currently unused. Use the debian version.

FROM lablup/common-py3-tensorflow-gpu:1.3-alpine as tf-binary
FROM lablup/common-cuda:alpine as cuda-libs


FROM lablup/kernel-python3:latest

# Install glibc
# TODO: implement!

# Install CUDA
COPY --from=cuda-libs /usr/local/cuda-8.0 /usr/local
RUN ln -s /usr/local/cuda-8.0 /usr/local/cuda && \
    ln -s /usr/local/cuda/lib64/libcudnn.so /usr/local/cuda/lib64/libcudnn.so.6.0
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/local/cuda/lib64/stubs" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

# Install TensorFlow and Keras
COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp
RUN pip install --no-cache-dir /tmp/tensorflow-*.whl && \
    pip install --no-cache-dir keras && \
    pip install --no-cache-dir h5py
RUN rm -f /tmp/*.whl

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.nvidia.enabled="yes" \
      com.nvidia.cuda.version="8.0.61" \
      com.nvidia.volumes.needed="nvidia_driver" \
      io.sorna.timeout="0" \
      io.sorna.maxmem="8g" \
      io.sorna.maxcores="4" \
      io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      io.sorna.features="batch query uid-match user-input"


# vim: ft=dockerfile sts=4 sw=4 et tw=0
