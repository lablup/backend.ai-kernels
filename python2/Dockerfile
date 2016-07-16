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
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN apt-get update && apt-get build-dep -y python
RUN apt-get install -y git-core wget libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev
ADD install-python.sh /home/sorna/install-python.sh
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
ADD install-python-packages.sh /home/sorna/install-python-packages.sh
RUN chmod +x /home/sorna/*.sh
USER work
RUN /home/sorna/install-python-packages.sh

# Additional packages
USER root
ENV MPLCONFIGDIR /home/sorna/.matplotlib
RUN mkdir /home/sorna/.matplotlib
RUN chown -R work:work /home/sorna/.matplotlib
ADD install-python-packages2.sh /home/sorna/install-python-packages2.sh
RUN chmod +x /home/sorna/install-python-packages2.sh
USER work
RUN /home/sorna/install-python-packages2.sh

# Sorna Media support
USER root
ADD *.whl /tmp
ADD install-sorna-media.sh /home/sorna/
RUN chmod +x /home/sorna/install-sorna-media.sh
ADD matplotlibrc /home/sorna/.matplotlib/
USER work
RUN /home/sorna/install-sorna-media.sh

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
CMD /home/sorna/run.sh
