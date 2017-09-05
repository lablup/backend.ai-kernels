FROM lablup/common-py3-tensorflow-cpu:1.3-debian as tf-binary


FROM lablup/kernel-base-python3-minimal:debian

# Install TensorFlow
COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp
RUN pip install /tmp/tensorflow-*.whl

# Install Keras
RUN pip install keras
RUN pip install h5py

COPY tensorflow.yml /home/sorna/policy.yml

LABEL io.sorna.timeout="600"
LABEL io.sorna.maxmem="8g"
LABEL io.sorna.maxcores="4"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC"
LABEL io.sorna.features "batch query uid-match user-input"
