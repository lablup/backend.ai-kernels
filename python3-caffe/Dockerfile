FROM ubuntu:16.04
MAINTAINER Joongi Kim "joongi@lablup.com"

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
ENV SORNA_PYTHON_VERSION 3.5.3
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; \
    echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install -y --only-upgrade tzdata
RUN apt-get install -y build-essential git-core wget ca-certificates libseccomp2 \
		       libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev tk-dev
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
RUN apt-get update
RUN apt-get install -y pkg-config
RUN apt-get install -y libjpeg-dev libpng-dev
RUN apt-get install -y libfreetype6-dev libblas-dev liblapack-dev libatlas-dev gfortran
USER work
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist six python-dateutil
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

# Install caffe
USER root
RUN apt-get update
RUN apt-get install -y libprotobuf-dev libleveldb-dev libsnappy-dev libopencv-dev libhdf5-dev libhdf5-serial-dev protobuf-compiler
RUN apt-get install -y --no-install-recommends libboost-all-dev
RUN apt-get install -y libgflags-dev libgoogle-glog-dev liblmdb-dev

USER root
ADD install-caffe-deps.sh /home/sorna/install-caffe-deps.sh
RUN chmod +x /home/sorna/install-caffe-deps.sh
USER work
RUN /home/sorna/install-caffe-deps.sh

USER root
RUN apt-get install -y libatlas-dev libatlas-base-dev libblas-dev
RUN ldconfig
USER work
RUN cd /home/sorna/caffe-install; make all -j2
USER root
RUN ln -s /usr/lib/x86_64-linux-gnu/libboost_python-py35.so /usr/lib/x86_64-linux-gnu/libboost_python-py3.so
RUN ln -s /usr/lib/x86_64-linux-gnu/libboost_python-py35.a /usr/lib/x86_64-linux-gnu/libboost_python-py3.a
RUN ln -s /usr/lib/x86_64-linux-gnu/libboost_python-py3.so /usr/lib/x86_64-linux-gnu/libboost_python3.so
RUN ln -s /usr/lib/x86_64-linux-gnu/libboost_python-py3.a /usr/lib/x86_64-linux-gnu/libboost_python3.a
RUN ldconfig
USER work
RUN /home/sorna/pyenv-run.sh pip install -U "protobuf>=3.0"
RUN cd /home/sorna/caffe-install; make pycaffe
ENV PYTHONPATH=/home/sorna/caffe-install/python:.

# Install Sorna Media support
USER root
ADD sorna_media-*.whl /tmp
ADD matplotlibrc /home/sorna/.matplotlib/
USER work
RUN /home/sorna/pyenv-run.sh pip install /tmp/sorna_media-*.whl

# Secure installation scripts
USER root
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
EXPOSE 2000 2001

LABEL io.sorna.timeout="180"
LABEL io.sorna.maxmem="1g"
LABEL io.sorna.maxcores="4"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,NPROC"
LABEL io.sorna.version="2"

USER work
ENV PYTHONUNBUFFERED 1
CMD /home/sorna/run.sh
