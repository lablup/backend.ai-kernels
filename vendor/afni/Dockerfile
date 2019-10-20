FROM lablup/neuro:ubuntu18.04

ENV DEBIAN_FRONTEND noninteractive

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
          python-dev \
          python-matplotlib \
          python-numpy \
          python-scipy \
          python-qt4 \
          python-wxgtk3.0 \
          python-rpy2 \
          python-tk \
          python-mpltoolkits.basemap \
          r-base \
          git-annex-standalone \
          tcsh \
          vim \
          rsync \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

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
#COPY [".", "$AFNI_ROOT/"]
RUN git clone https://github.com/afni/afni.git $AFNI_ROOT

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

RUN apt update && \
    apt install -y software-properties-common && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' && \
    apt update && \
    apt install -y r-base

RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir ipywidgets && \
    python3 -m pip install --no-cache-dir ipyparallel && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    python3 -m pip install --no-cache-dir jupyterthemes && \    
    python3 -m pip install --no-cache-dir jupyter-js-widgets-nbextension && \
    python3 -m pip install --no-cache-dir jupyter_contrib_nbextensions && \
    python3 -m pip install --no-cache-dir jupyter_nbextensions_configurator && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# python alternative support
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

RUN \
    Rscript -e "install.packages(c('rzmq','repr','IRkernel','IRdisplay'),type = 'source')" && \
    Rscript -e "install.packages(c('slidify','ggplot2','devtools'))" && \    
    Rscript -e "IRkernel::installspec(displayname = 'R-base 3.6 On Backend.AI' , user = FALSE)" && \
    Rscript -e "install.packages(c('KoNLP'))" && \
    rm -rf /root/.local/share/jupyter && \
    jupyter kernelspec list && \
    cat /usr/local/share/jupyter/kernels/ir/kernel.json 

#RUN Rscript -e "install.packages('IRkernel')" && \
#    Rscript -e "IRkernel::installspec()" && \
#    mv /root/.local/share/jupyter/kernels/ir /usr/local/share/jupyter/kernels && \
#    rm -rf /root/.local/share/jupyter

# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "AFNI for Python 3.6 on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="r" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="jupyter:http:8080,jupyterlab:http:8090"
COPY policy.yml /etc/backend.ai/jail/policy.yml
