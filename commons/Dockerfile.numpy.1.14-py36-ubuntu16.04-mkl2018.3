FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/kernel-base:ubuntu16.04-mkl2018.3

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        cmake \
        cpio \
        gcc \
        g++ \
        gfortran \
        git \
        man \
        wget

COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

RUN echo "/opt/intel/mkl/lib/intel64" >> /etc/ld.so.conf.d/intel.conf && \
  ldconfig && \
  echo ". /opt/intel/bin/compilervars.sh intel64" >> /etc/bash.bashrc

# Build numpy with MKL
RUN pip install Cython wheel

RUN cd /tmp && \
  git clone -q https://github.com/numpy/numpy -b "v1.15.2" numpy && \
  cd numpy && \
  echo "\n[mkl]" > site.cfg && \
  echo "include_dirs = /opt/intel/mkl/include/intel64/" >> site.cfg && \
  echo "library_dirs = /opt/intel/mkl/lib/intel64/" >> site.cfg && \
  echo "mkl_libs = mkl_rt" >> site.cfg && \
  echo "lapack_libs =" >> site.cfg && \
  echo "search_static_first = false" >> site.cfg && \
  python setup.py build --fcompiler=gnu95
RUN cd /tmp/numpy && \
    python setup.py bdist_wheel && \
    ls -l dist

RUN cd /tmp/numpy && pip install dist/*.whl

# Build scipy with numpy with MKL
RUN cd /tmp && \
  git clone https://github.com/scipy/scipy -b "v1.1.0" scipy && \
  cd scipy && \
  python setup.py bdist_wheel && \
  ls -l dist

# The result artifacts are:
#   /tmp/numpy/dist/numpy-xxx.whl
#   /tmp/scipy/dist/scipy-xxx.whl

# vim: ft=dockerfile
