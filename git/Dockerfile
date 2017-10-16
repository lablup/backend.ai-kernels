FROM lablup/kernel-base-python3-wheels:latest as wheel-builds
RUN ls -lh /root


# -------------------
FROM lablup/kernel-base-python3-minimal:latest

# Install Git extras and some Python packages
RUN apk add --no-cache --virtual .sorna-deps \
        libgit2 libffi wget curl bash git nano vim \
        libjpeg libpng freetype openblas lapack \
    && apk add --no-cache --virtual .build-deps \
        build-base libffi-dev cmake \
    # Hack for numpy
    && ln -s /usr/include/locale.h /usr/include/xlocale.h \
    && mkdir -p /tmp/git-flow && cd /tmp/git-flow \
    && wget --no-check-certificate -q https://raw.githubusercontent.com/petervanderdoes/gitflow-avh/develop/contrib/gitflow-installer.sh \
    && bash gitflow-installer.sh install stable \
    && cd /tmp && rm -rf /tmp/git-flow \
    && mkdir -p /tmp/libgit-build && cd /tmp/libgit-build \
    && wget https://github.com/libgit2/libgit2/archive/v0.26.0.tar.gz \
    && tar xzf v0.26.0.tar.gz \
    && cd libgit2-0.26.0/ \
    && cmake . \
    && make -j8 \
    && make install \
    && cd /tmp && rm -rf /tmp/libgit-build

COPY --from=wheel-builds /root/numpy*.whl /root/pandas*.whl /root/matplotlib*.whl /tmp/
RUN pip install --no-cache-dir \
        pygit2 \
        /tmp/numpy*.whl \
        /tmp/pandas*.whl \
        /tmp/matplotlib*.whl

# Install Sorna Media support
COPY *.whl /tmp
RUN pip install --no-cache-dir /tmp/sorna_media-*.whl \
    && rm -f /tmp/*.whl
RUN apk del .build-deps

# Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR /home/sorna/.matplotlib
RUN mkdir /home/sorna/.matplotlib
COPY matplotlibrc /home/sorna/.matplotlib/
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
    && python /tmp/matplotlib-fontcache.py \
    && rm /tmp/matplotlib-fontcache.py

# Configure the shell environment
ENV TERM xterm
ENV USER work
ENV SHELL /bin/bash

RUN echo 'set convert-meta off' >> /etc/inputrc; \
    echo 'set hlsearch incsearch sts=4 sw=4 et' >> /etc/vim/vimrc.local; \
    echo 'set enc=utf8 tenc=utf8' >> /etc/vim/vimrc.local; \
    rm -f /usr/bin/vi && ln -s /usr/bin/vim /usr/bin/vi

COPY run.py /home/sorna/run.py
COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "query tty uid-match"

# vim: sts=4 sw=4 et
