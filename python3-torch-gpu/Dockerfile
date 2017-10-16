FROM lablup/common-cuda:alpine as cuda-libs


FROM lablup/kernel-python3:latest

# Install CUDA
COPY --from=cuda-libs /usr/local/cuda-8.0 /usr/local
RUN ln -s /usr/local/cuda-8.0 /usr/local/cuda && \
    ln -s /usr/local/cuda/lib64/libcudnn.so /usr/local/cuda/lib64/libcudnn.so.6.0 && \
    mkdir -p /etc/ld.so.conf.d && \
    echo "/usr/local/cuda/lib64" > /etc/ld.so.conf.d/cuda.conf && \
    echo "/usr/local/nvidia/lib64" > /etc/ld.so.conf.d/nvidia.conf && \
    ldconfig

# Install Torch
RUN pip install --no-cache-dir \
        http://download.pytorch.org/whl/cu80/torch-0.2.0.post3-cp36-cp36m-manylinux1_x86_64.whl && \
    rm -rf /root/.cache

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.timeout="0" \
      io.sorna.maxmem="8g" \
      io.sorna.maxcores="4" \
      io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      io.sorna.features="batch query uid-match user-input"


# vim: ft=dockerfile sts=4 sw=4 et tw=0
