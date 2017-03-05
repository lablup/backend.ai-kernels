FROM lablup/kernel-base-python3-minimal:latest

LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,NPROC"

COPY *.whl /tmp

# Prepare build environment and libraries
RUN apk add --no-cache --virtual .sorna-deps \
        libgfortran libquadmath libjpeg libpng freetype openblas lapack \
    && apk add --no-cache --virtual .pybuild-deps \
        gcc g++ gfortran make linux-headers libc-dev freetype-dev openblas-dev \
    # Hack for numpy
    && ln -s /usr/include/locale.h /usr/include/xlocale.h

# Install science packages
RUN pip install --no-cache-dir cython \
    && pip install --no-cache-dir numpy \
    && pip install --no-cache-dir matplotlib bokeh \
    && pip install --no-cache-dir pandas \
    && pip install --no-cache-dir scipy seaborn
RUN apk add --no-cache --virtual .sorna-deps jpeg libpng tiff \
    && apk add --no-cache --virtual .pybuild-deps jpeg-dev libpng-dev tiff-dev \
    && pip install --no-cache-dir pillow \
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
COPY matplotlibrc /home/sorna/.matplotlib/
RUN chown -R work:work /home/sorna/.matplotlib \
    && echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
    && python /tmp/matplotlib-fontcache.py \
    && rm /tmp/matplotlib-fontcache.py

COPY run.py /home/sorna/run.py

USER work
CMD ["/home/sorna/jail", "python3", "/usr/local/bin/python3", "/home/sorna/run.py"]

# vim: sts=4 sw=4 et ft=dockerfile
