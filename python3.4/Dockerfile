FROM ubuntu:14.04
MAINTAINER Joongi Kim "joongi@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for Python and REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna
ADD run.py /home/sorna/run.py
ADD run.sh /home/sorna/run.sh

ENV HOME /home/work
WORKDIR /home/work

# Install Python 3.4
RUN apt-get update && apt-get build-dep -y python3
RUN apt-get install -y wget libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev
ADD install-python.sh /home/sorna/install-python.sh
ADD install-python-packages.sh /home/sorna/install-python-packages.sh
RUN chmod +x /home/sorna/*.sh

USER work
RUN /home/sorna/install-python.sh

# Install common Python packages
ENV PYENV_ROOT /home/work/.pyenv
ENV PATH /home/work/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN /home/sorna/install-python-packages.sh

# Secure installation scripts
RUN chown root:root /home/sorna/*.sh
RUN chmod 600 /home/sorna/*.sh
RUN chmod 755 /home/sorna/run.sh

VOLUME ["/home/work"]
EXPOSE 2001

CMD /home/sorna/run.sh
