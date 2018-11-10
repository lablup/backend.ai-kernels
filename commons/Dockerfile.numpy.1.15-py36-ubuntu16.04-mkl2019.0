FROM lablup/kernel-base:python3.6 as python-binary
FROM lablup/kernel-base:ubuntu16.04-mkl2019

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

RUN update-alternatives --install /usr/lib/x86_64-linux-gnu/libblas.so     \
           libblas.so-x86_64-linux-gnu      /opt/intel/mkl/lib/intel64/libmkl_rt.so 50 && \
    update-alternatives --install /usr/lib/x86_64-linux-gnu/libblas.so.3   \
           libblas.so.3-x86_64-linux-gnu    /opt/intel/mkl/lib/intel64/libmkl_rt.so 50 && \
    update-alternatives --install /usr/lib/x86_64-linux-gnu/liblapack.so   \
           liblapack.so-x86_64-linux-gnu    /opt/intel/mkl/lib/intel64/libmkl_rt.so 50 && \
    update-alternatives --install /usr/lib/x86_64-linux-gnu/liblapack.so.3 \		     
           liblapack.so.3-x86_64-linux-gnu  /opt/intel/mkl/lib/intel64/libmkl_rt.so 50

RUN echo "/opt/intel/mkl/lib/intel64" >> /etc/ld.so.conf.d/intel.conf && \
    ldconfig && \
    echo ". /opt/intel/bin/compilervars.sh intel64" >> /etc/bash.bashrc

# Build numpy with MKL
RUN pip install Cython wheel

RUN  cd /tmp && \
     git clone -q https://github.com/numpy/numpy -b "v1.15.2" numpy && \
     cd numpy && \
     cp site.cfg.example site.cfg && \
     echo "\n[mkl]" > site.cfg && \
     echo "include_dirs = /opt/intel/mkl/include/intel64/" >> site.cfg && \
     echo "library_dirs = /opt/intel/mkl/lib/intel64/" >> site.cfg && \
     echo "mkl_libs = mkl_rt" >> site.cfg && \
     echo "lapack_libs =" >> site.cfg && \ 
     echo "search_static_first = false" >> site.cfg && \
#     touch /tmp/numpy/.config/pip/pip.conf && \
#     echo "\n[install]" >> /tmp/numpy/.config/pip/pip.conf && \
#     echo "no-binary = numpy,scipy" >> /tmp/numpy/.config/pip/pip.conf && \
#     pip install --no-binary :all: numpy && \
     python setup.py build --fcompiler=gnu95 && \
     python setup.py bdist_wheel && \
     pip install dist/*.whl && \
     ls -l dist

# Build scipy with numpy with MKL
RUN cd /tmp && \
    git clone https://github.com/scipy/scipy -b "v1.1.0" scipy && \
    cd scipy && \
#    pip install --no-binary :all: scipy && \
    python setup.py bdist_wheel && \
    ls -l dist

# The result artifacts are:
#   /tmp/numpy/dist/numpy-xxx.whl
#   /tmp/scipy/dist/scipy-xxx.whl

# vim: ft=dockerfile
