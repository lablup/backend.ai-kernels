FROM ubuntu:16.04
MAINTAINER Jeongkyu Shin "jshin@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for Python and REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna

ENV DEBIAN_FRONTEND noninteractive
ENV HOME /home/work
WORKDIR /home/work

# Install Python
USER root
ENV SORNA_PYTHON_VERSION 3.6.0
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get build-dep -y python3
RUN apt-get install -y git-core wget libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev tk-dev
ADD install-python.sh /home/sorna/install-python.sh
ADD pyenv-run.sh /home/sorna/pyenv-run.sh
RUN chmod +x /home/sorna/*.sh
USER work
ENV PYENV_ROOT /home/sorna/.pyenv
ENV PATH /home/sorna/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN git clone https://github.com/yyuu/pyenv /home/sorna/.pyenv
RUN /home/sorna/install-python.sh

# Install common Python packages
USER root
RUN apt-get install -y pkg-config
RUN apt-get install -y libjpeg-dev libpng-dev
RUN apt-get install -y libfreetype6-dev libblas-dev liblapack-dev libatlas-dev gfortran
USER work
# matplotlib may install older version of dateutil; we first install it.
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist six python-dateutil nose
RUN /home/sorna/pyenv-run.sh pip install numpy scipy matplotlib bokeh
RUN /home/sorna/pyenv-run.sh pip install scikit-learn scikit-image
RUN /home/sorna/pyenv-run.sh pip install cvxpy seaborn
RUN /home/sorna/pyenv-run.sh pip install pandas networkx
RUN /home/sorna/pyenv-run.sh pip install pillow sklearn

# Additional packages
USER root
ENV MPLCONFIGDIR /home/sorna/.matplotlib
RUN mkdir /home/sorna/.matplotlib
RUN chown -R work:work /home/sorna/.matplotlib
USER work
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py
RUN /home/sorna/pyenv-run.sh python /tmp/matplotlib-fontcache.py
RUN rm /tmp/matplotlib-fontcache.py

# Install Sorna Media support
USER work
ADD matplotlibrc /home/sorna/.matplotlib/
ADD sorna_media-*.whl /tmp
RUN /home/sorna/pyenv-run.sh pip install /tmp/sorna_media-*.whl

# Install Theano
USER work
RUN /home/sorna/pyenv-run.sh pip install Theano

# Secure installation scripts
USER root
RUN apt-get install -y libseccomp2
ADD run.py /home/sorna/run.py
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
ADD patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

VOLUME ["/home/work"]
EXPOSE 2001

LABEL io.sorna.timeout="30"
LABEL io.sorna.maxmem="512m"
LABEL io.sorna.maxcores="4"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS"

USER work
ENV PYTHONUNBUFFERED 1
CMD /home/sorna/run.sh
