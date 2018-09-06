FROM lablup/common-cuda:cuda9.0-cudnn7.1 as cuda-libs
FROM lablup/kernel-python:3.6-debian

ENV LANG=C.UTF-8

# Install CUDA
COPY --from=cuda-libs /usr/local/cuda-9.0 /usr/local/cuda-9.0
RUN ln -s /usr/local/cuda-9.0 /usr/local/cuda && \
    ln -s /usr/local/cuda/lib64/libcudnn.so /usr/local/cuda/lib64/libcudnn.so.6.0
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/local/cuda/lib64/stubs" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

RUN pip install --upgrade pip==9.0.1 && \
    pip install setuptools==38.2.4 && \
    pip install cupy==2.2.0 chainer==3.2.0

# Below scripts are already executed in python/Dockerfile.3.6-ubuntu
# Install kernel-runner scripts package
# RUN pip install --no-cache-dir "backend.ai-kernel-runner[python]~=1.0.4"

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.nvidia.enabled="yes" \
      com.nvidia.cuda.version="9.0.176" \
      com.nvidia.volumes.needed="nvidia_driver" \
      io.sorna.timeout="0" \
      io.sorna.maxmem="8g" \
      io.sorna.maxcores="4" \
      io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      io.sorna.features="batch query uid-match user-input"
