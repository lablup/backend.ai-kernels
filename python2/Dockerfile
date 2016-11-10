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
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get build-dep -y python
RUN apt-get install -y git-core wget libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev
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
RUN /home/sorna/pyenv-run.sh pip install python-dateutil
RUN /home/sorna/pyenv-run.sh pip install numpy matplotlib
RUN /home/sorna/pyenv-run.sh pip install scipy
RUN /home/sorna/pyenv-run.sh pip install scikit-learn scikit-image
RUN /home/sorna/pyenv-run.sh pip install cvxpy
RUN /home/sorna/pyenv-run.sh pip install pandas
RUN /home/sorna/pyenv-run.sh pip install seaborn
RUN /home/sorna/pyenv-run.sh pip install networkx
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist six

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
USER root
ADD *.whl /tmp
ADD matplotlibrc /home/sorna/.matplotlib/
USER work
RUN /home/sorna/pyenv-run.sh pip install /tmp/sorna_media-0.2.0-py2.py3-none-any.whl

# Secure installation scripts
USER root
ADD run.py /home/sorna/run.py
ADD run.sh /home/sorna/run.sh
# NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
RUN apt-get install -y libseccomp2
ADD jail /home/sorna/jail
ADD intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail

VOLUME ["/home/work"]
EXPOSE 2001

LABEL com.lablup.sorna.timeout="10"
LABEL com.lablup.sorna.maxmem="128m"

USER work
ENV PYTHONUNBUFFERED 1
ENV OPENBLAS_NUM_THREADS 1
CMD /home/sorna/run.sh
