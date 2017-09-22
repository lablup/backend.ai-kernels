FROM lablup/common-py3-tensorflow-cpu:1.3-alpine as tf-binary


FROM lablup/kernel-python3:latest

# Install TensorFlow and Keras
COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp
RUN pip install --no-cache-dir /tmp/tensorflow-*.whl && \
    pip install --no-cache-dir keras && \
    pip install --no-cache-dir h5py
RUN rm -f /tmp/*.whl

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.timeout="0" \
      io.sorna.maxmem="8g" \
      io.sorna.maxcores="4" \
      io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      io.sorna.features "batch query uid-match user-input"


# vim: ft=dockerfile sts=4 sw=4 et tw=0
