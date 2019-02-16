FROM lablup/kernel-base:jail as jail-builder
FROM lablup/kernel-base:hook as hook-builder
FROM lablup/common-numpy:1.15-py36-ubuntu16.04-mkl2019.0 as numpy-binary
FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/common-tensorflow:1.12-py36 as tf-binary

FROM lablup/kernel-base:ubuntu16.04-mkl2019
ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt update -y && \
    apt install -y \
        ca-certificates \
        wget \
        libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g \
	libmpdec2 \
        libssl1.0.0 \
	libssl-dev \
        libncursesw5 libtinfo5 libreadline6 \
	proj-bin \
        libgeos-dev \
        mime-support \
	gcc g++ \
        libproj-dev libgeos-dev \	
        libzmq3-dev libuv1

# Copy the whole Python from the docker library image
#COPY --from=numpy-binary /opt/intel/mkl /opt/intel/mkl
#COPY --from=numpy-binary /opt/intel/mkl/lib/intel64 /opt/intel/mkl/lib/intel64
#COPY --from=numpy-binary /opt/intel/mkl/include/intel64 /opt/intel/mkl/include/intel64

COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
# Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

# As we mostly have "manylinux" glibc-compatible binary packaes,
# we don't have to rebuild these!
COPY --from=numpy-binary /tmp/numpy/dist/numpy-*.whl /tmp
COPY --from=numpy-binary /tmp/scipy/dist/scipy*.whl /tmp
COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp
# Install TensorFlow build dependencies (ensure we have proper numpy)

# Prepare for building TensorFlwo wheel
RUN pip install --no-cache-dir wheel && \
    pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    pip install --no-cache-dir aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" && \
    pip install --no-cache-dir h5py && \
    pip install --no-cache-dir Cython && \
    pip install --no-cache-dir matplotlib bokeh && \
    pip install --no-cache-dir pyproj && \
    pip install --no-cache-dir Cartopy && \
    pip install --no-cache-dir wheel /tmp/*.whl && \
    pip install --no-cache-dir keras && \
    pip install --no-cache-dir ipython && \
    pip install --no-cache-dir pandas && \
    pip install --no-cache-dir seaborn && \
    pip install --no-cache-dir pillow && \
    pip install --no-cache-dir networkx cvxpy && \
    pip install --no-cache-dir scikit-learn scikit-image && \
    pip install --no-cache-dir pygments && \
    rm -f /tmp/*.whl

RUN apt-get install -y libseccomp2 gosu && \
    apt-get clean && \
    rm -r /var/lib/apt/lists /var/cache/apt/archives && \
    ln -s /usr/sbin/gosu /usr/sbin/su-exec && \
    mkdir /home/work && chmod 755 /home/work; \
    mkdir /home/backend.ai && chmod 755 /home/backend.ai
ADD entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY policy.yml /home/backend.ai/policy.yml

# Install jail
COPY --from=jail-builder /go/src/github.com/lablup/backend.ai-jail/backend.ai-jail /home/backend.ai/jail
COPY --from=hook-builder /root/backend.ai-hook/libbaihook.so /home/backend.ai/libbaihook.so
ENV LD_PRELOAD /home/backend.ai/libbaihook.so

# Install kernel-runner scripts package
RUN pip install --no-cache-dir "backend.ai-kernel-runner[python]~=1.4.0"

# Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR /home/backend.ai/.matplotlib
RUN mkdir /home/backend.ai/.matplotlib
COPY matplotlibrc /home/backend.ai/.matplotlib/
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
    && python /tmp/matplotlib-fontcache.py \
    && rm /tmp/matplotlib-fontcache.py

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL ai.backend.timeout="0" \
      ai.backend.maxmem="8g" \
      ai.backend.maxcores="4" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input"

CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "python"]

# vim: ft=dockerfile