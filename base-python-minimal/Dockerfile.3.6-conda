FROM lablup/kernel-base:conda

ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED 1
ENV PATH /opt/conda/bin:$PATH
ENV ACCEPT_INTEL_PYTHON_EULA=yes

RUN apt-get update --fix-missing
RUN install_packages \
        ca-certificates \
        libexpat1 libgdbm3 bzip2 libffi6 libsqlite3-0 liblzma5 zlib1g \
        libssl1.0.0 libmpdec2 \
        libncursesw5 libtinfo5 libreadline6 \
        mime-support \
        libzmq3 libuv0.10

RUN conda config --add channels intel && \
    conda install -y -q -c intel intelpython3_full=2018.0.2 && \
    conda install python=3 && \
    conda clean --all && \
    apt-get update -qqq && \
    apt-get install -y -q g++ && \
    apt-get autoremove && \
    rm -rf /var/lib/apt/lists/*

ENV TINI_VERSION v0.16.1
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /usr/bin/tini
RUN chmod +x /usr/bin/tini

ENTRYPOINT [ "/usr/bin/tini", "--" ]


# As we mostly have "manylinux" glibc-compatible binary packaes,
# we don't have to rebuild these!
RUN conda update -n base conda && \
    conda install pyzmq simplejson msgpack-python uvloop tabulate six && \
    conda install -c hcc aiozmq && \
    pip install --no-cache-dir dataclasses namedlist "python-dateutil>=2"

 # Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'

CMD [ "/bin/bash" ]
# vim: ft=dockerfile
