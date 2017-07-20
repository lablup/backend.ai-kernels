FROM ubuntu:16.04
MAINTAINER DevOps "devops@lablup.com"

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

# Set up the base environment.
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install -y --only-upgrade tzdata
RUN apt-get install -y build-essential git-core curl wget ca-certificates libseccomp2 libzmq3-dev

# Install common Python dependencies
RUN apt-get install -y libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev tk-dev
RUN apt-get install -y pkg-config libjpeg-dev libpng-dev
RUN apt-get install -y libjpeg-dev libpng-dev
RUN apt-get install -y libfreetype6-dev libblas-dev liblapack-dev libatlas-dev gfortran
CMD /home/sorna/run.sh

# Install Python installer (pyenv)
USER work
ENV PYENV_ROOT /home/sorna/.pyenv
ENV PATH /home/sorna/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN git clone https://github.com/yyuu/pyenv /home/sorna/.pyenv

# Install Python
ENV PYTHONUNBUFFERED 1
ENV SORNA_PYTHON_VERSION 3.6.0
ADD install-python.sh /home/sorna/install-python.sh
ADD pyenv-run.sh      /home/sorna/pyenv-run.sh
RUN /home/sorna/install-python.sh

# Install common Python packages
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist simplejson six "python-dateutil>2"
RUN /home/sorna/pyenv-run.sh pip install numpy scipy matplotlib bokeh
RUN /home/sorna/pyenv-run.sh pip install scikit-learn scikit-image
RUN /home/sorna/pyenv-run.sh pip install pandas networkx cvxpy seaborn
RUN /home/sorna/pyenv-run.sh pip install pillow sklearn

# Matplotlib configuration
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

# Install TensorFlow
USER work
ADD tensorflow-1.0.0-cp36-cp36m-linux_x86_64.whl /tmp
RUN /home/sorna/pyenv-run.sh pip install /tmp/tensorflow-*.whl

# Install Keras
RUN /home/sorna/pyenv-run.sh pip install keras
RUN /home/sorna/pyenv-run.sh pip install h5py

# Secure installation scripts
USER root
COPY run.sh /home/sorna/run.sh
# taken from "make manylinux" in lablup/sorna-jail repo.
COPY jail /home/sorna/jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail
COPY patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

VOLUME ["/home/work"]
EXPOSE 2000 2001

LABEL io.sorna.timeout="600"
LABEL io.sorna.maxmem="8g"
LABEL io.sorna.maxcores="4"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC"
LABEL io.sorna.version="2"

COPY run.py /home/sorna/run.py
COPY tensorflow.yml /home/sorna/tensorflow.yml
USER work
