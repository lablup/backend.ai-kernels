FROM ubuntu:18.04

# Install Java
RUN apt-get update && \
    apt-get install -y default-jdk wget gnupg2

# Install Scala
RUN wget http://scala-lang.org/files/archive/scala-2.12.1.deb && \
    dpkg -i scala-2.12.1.deb && \
    apt-get update && apt-get install scala

# SBT
RUN echo "deb https://dl.bintray.com/sbt/debian /" | \
        tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
        --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get update && apt-get install -y sbt

LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="octave" \
      ai.backend.runtime-path="/usr/bin/scala" \
      ai.backend.service-ports=""
COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile
