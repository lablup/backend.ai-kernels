FROM lablup/kernel-base-python3-minimal:latest

LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,NPROC"

COPY blas-3.7.0.tgz /tmp/
COPY lapack-3.7.0.tgz /tmp/
COPY make-lapack.inc /tmp/
ADD *.whl /tmp

ENV BLAS /usr/local/lib/libfblas.a
ENV LAPACK /usr/local/lib/liblapack.a

# Prepare build environment and libraries
RUN apk add --no-cache --virtual .sorna-deps \
        libgfortran libquadmath libjpeg libpng freetype \
    && apk add --no-cache --virtual .pybuild-deps \
        gcc g++ gfortran make linux-headers libc-dev freetype-dev \
    # Hack for numpy
    && ln -s /usr/include/locale.h /usr/include/xlocale.h

RUN cd /tmp \
    # Build BLAS
    && tar xzf blas-3.7.0.tgz \
    && cd BLAS-3.7.0 \
    && gfortran -O3 -std=legacy -m64 -fno-second-underscore -fPIC -c *.f \
    && ar r libfblas.a *.o \
    && ranlib libfblas.a \
    && rm -rf *.o \
    && mv libfblas.a /usr/local/lib \
    # Build LAPACK
    && cd /tmp \
    && tar xzf lapack-3.7.0.tgz \
    && mv make-lapack.inc lapack-3.7.0/make.inc \
    && cd lapack-3.7.0 \
    && make lapacklib -j \
    && mv liblapack.a /usr/local/lib \
    # Clean BLAS/LAPACK sources and build files
    && cd /tmp \
    && rm -rf /tmp/blas* /tmp/lapack*

# Install science packages
RUN pip install --no-cache-dir cython \
    && pip install --no-cache-dir numpy \
    && pip install --no-cache-dir matplotlib bokeh \
    && pip install --no-cache-dir pandas \
    && pip install --no-cache-dir scipy seaborn
RUN apk add --no-cache --virtual .sorna-deps jpeg libpng tiff \
    && apk add --no-cache --virtual .pybuild-deps jpeg-dev libpng-dev tiff-dev
RUN pip install --no-cache-dir pillow \
    && pip install --no-cache-dir scikit-learn scikit-image \
    && pip install --no-cache-dir networkx cvxpy
# Install Sorna Media support
RUN pip install --no-cache-dir /tmp/sorna_media-0.3.0-py2.py3-none-any.whl \
    && rm -f /tmp/*.whl

# Clean up build dependencies
RUN apk del .pybuild-deps

# Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR /home/sorna/.matplotlib
RUN mkdir /home/sorna/.matplotlib
ADD matplotlibrc /home/sorna/.matplotlib/
RUN chown -R work:work /home/sorna/.matplotlib \
    && echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
    && python /tmp/matplotlib-fontcache.py \
    && rm /tmp/matplotlib-fontcache.py

ADD run.py /home/sorna/run.py

USER work
CMD ["/home/sorna/jail", "python3", "/usr/local/bin/python3", "/home/sorna/run.py"]

# vim: sts=4 sw=4 et ft=Dockerfile
