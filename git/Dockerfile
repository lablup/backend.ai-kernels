FROM lablup/kernel-base-python3-minimal:latest

# Install Git extras and some Python packages
USER root
RUN apk add --no-cache --virtual .sorna-deps \
        libgit2 libffi wget bash git nano vim \
        libgfortran libquadmath libjpeg libpng freetype openblas lapack \
    && apk add --no-cache --virtual .build-deps \
        build-base libc-dev libffi-dev cmake \
        gfortran linux-headers libc-dev freetype-dev openblas-dev \
    # Hack for numpy
    && ln -s /usr/include/locale.h /usr/include/xlocale.h \
    && mkdir -p /tmp/git-flow && cd /tmp/git-flow \
    && wget --no-check-certificate -q https://raw.githubusercontent.com/petervanderdoes/gitflow-avh/develop/contrib/gitflow-installer.sh \
        && bash gitflow-installer.sh install stable \
    && cd /tmp && rm -rf /tmp/git-flow \
    && mkdir -p /tmp/libgit-build && cd /tmp/libgit-build \
        && wget https://github.com/libgit2/libgit2/archive/v0.25.0.tar.gz \
        && tar xzf v0.25.0.tar.gz \
        && cd libgit2-0.25.0/ \
        && cmake . \
        && make \
        && make install \
    && cd /tmp && rm -rf /tmp/libgit-build \
    && pip install --no-cache-dir pygit2 aiozmq numpy matplotlib

# Install Sorna Media support
COPY *.whl /tmp
RUN pip install --no-cache-dir /tmp/sorna_media-0.3.0-py2.py3-none-any.whl \
    && rm -f /tmp/*.whl
RUN apk del .build-deps

# Configure the shell to use Unicode characeters
ENV TERM xterm
RUN echo 'set convert-meta off' >> /etc/inputrc; \
    echo 'set hlsearch incsearch sts=4 sw=4 et' >> /etc/vim/vimrc.local; \
    echo 'set enc=utf8 tenc=utf8' >> /etc/vim/vimrc.local; \
    rm -f /usr/bin/vi && ln -s /usr/bin/vim /usr/bin/vi

EXPOSE 2000 2001 2002 2003
LABEL io.sorna.mode="query+pty"

COPY run.py /home/sorna/run.py
COPY jail /home/sorna/
COPY intra-jail /home/sorna/
ENV USER work
ENV SHELL /bin/bash
USER work
CMD ["/home/sorna/jail", "git", "/usr/local/bin/python3", "/home/sorna/run.py"]
