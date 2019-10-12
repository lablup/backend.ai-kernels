FROM ubuntu:bionic

# https://bugs.debian.org/830696 (apt uses gpgv by default in newer releases, rather than gpg)
RUN set -x \
	&& apt-get update \
	&& { \
		which gpg \
		|| apt-get install -y --no-install-recommends gnupg \
	; } \
# Ubuntu includes "gnupg" (not "gnupg2", but still 2.x), but not dirmngr, and gnupg 2.x requires dirmngr
# so, if we're not running gnupg 1.x, explicitly install dirmngr too
	&& { \
		gpg --version | grep -q '^gpg (GnuPG) 1\.' \
		|| apt-get install -y --no-install-recommends dirmngr \
	; } \
	&& rm -rf /var/lib/apt/lists/*

# apt-key is a bit finicky during "docker build" with gnupg 2.x, so install the repo key the same way debian-archive-keyring does (/etc/apt/trusted.gpg.d)
# this makes "apt-key list" output prettier too!
RUN set -x \
	&& export GNUPGHOME="$(mktemp -d)" \
	&& gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys DD95CC430502E37EF840ACEEA5D32F012649A5A9 \
	&& gpg --batch --export DD95CC430502E37EF840ACEEA5D32F012649A5A9 > /etc/apt/trusted.gpg.d/neurodebian.gpg \
	&& rm -rf "$GNUPGHOME" \
	&& apt-key list | grep neurodebian

RUN { \
	echo 'deb http://neuro.debian.net/debian bionic main'; \
	echo 'deb http://neuro.debian.net/debian data main'; \
	echo '#deb-src http://neuro.debian.net/debian-devel bionic main'; \
} > /etc/apt/sources.list.d/neurodebian.sources.list

# Minimalistic package to assist with freezing the APT configuration
# which would be coming from neurodebian repo.
# Also install and enable eatmydata to be used for all apt-get calls
# to speed up docker builds.
RUN set -x \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends neurodebian-freeze eatmydata \
	&& ln -s /usr/bin/eatmydata /usr/local/bin/apt-get \
	&& rm -rf /var/lib/apt/lists/*

FROM neurodebian:bionic
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y -qq \
    && apt-get install -yq --no-install-recommends \
          ca-certificates \
          curl \
          g++ \
          gcc \
          git \
          libglib2.0-dev \
          libglu1-mesa-dev \
          libglw1-mesa-dev \
          libgsl-dev \
          libmotif-dev \
          libxi-dev \
          libxmhtml-dev \
          libxmu-dev \
          libxpm-dev \
          libxt-dev \
          m4 \
          r-base \
          git-annex-standalone \
          tcsh \
          vim \
          rsync \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        vim-tiny zip unzip \
        python3 python3-pip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        gcc g++ && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8

RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir versioneer==0.17 && \
    python3 -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python3 -m pip install --no-cache-dir qy4 && \
    python3 -m pip install --no-cache-dir wxgtk3.0 && \
    python3 -m pip install --no-cache-dir rpy2 && \
    python3 -m pip install --no-cache-dir tk && \    
    python3 -m pip install --no-cache-dir pandas && \
    python3 -m pip install --no-cache-dir seaborn && \
    python3 -m pip install --no-cache-dir pillow && \
    python3 -m pip install --no-cache-dir networkx cvxpy && \
    python3 -m pip install --no-cache-dir scikit-learn scikit-image && \
    python3 -m pip install --no-cache-dir pygments && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2


# Install some dependencies for python 3 (including testing dependencies)
RUN curl -fsSL https://bootstrap.pypa.io/get-pip.py | python3 - --no-cache-dir \
    # Add some dependencies for testing and coverage calculation
    && pip3 install --no-cache-dir \
            codecov \
            pytest \
            pytest-cov \
            numpy \
            pandas \
            nibabel \
            datalad \
            pytest-parallel \
            autopep8 \
            black \
            pdbpp

# Copy AFNI source code. This can invalidate the build cache.
ARG AFNI_ROOT=/opt/afni
COPY [".", "$AFNI_ROOT/"]

ARG AFNI_MAKEFILE_SUFFIX=linux_ubuntu_16_64
ARG AFNI_WITH_COVERAGE="0"

WORKDIR "$AFNI_ROOT/src"
RUN \
    if [ "$AFNI_WITH_COVERAGE" != "0" ]; then \
      echo "Adding testing and coverage components" \
      && sed -i 's/# CPROF = /CPROF =  -coverage /' Makefile.$AFNI_MAKEFILE_SUFFIX ;\
      fi \
    && make -f  Makefile.$AFNI_MAKEFILE_SUFFIX afni_src.tgz \
    && mv afni_src.tgz .. \
    && cd .. \
    \
    # Empty the src directory, and replace with the contents of afni_src.tgz
    && rm -rf src/ && mkdir src \
    && tar -xzf afni_src.tgz -C $AFNI_ROOT/src --strip-components=1 \
    && rm afni_src.tgz \
    \
    # Build AFNI.
    && cd src \
    && cp Makefile.$AFNI_MAKEFILE_SUFFIX Makefile \
    # Clean in case there are some stray object files
    && make cleanest \
    && make itall  | tee /build_log.txt \
    && mv $AFNI_MAKEFILE_SUFFIX $AFNI_ROOT/abin

ENV PATH="$AFNI_ROOT/abin:$PATH"

# set non interactive backend for matplotlib
RUN mkdir -p /root/.config/matplotlib \
    && echo "backend: Agg" > /root/.config/matplotlib/matplotlibrc

WORKDIR "$AFNI_ROOT"

# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "NeuroDebian on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"
COPY policy.yml /etc/backend.ai/jail/policy.yml
