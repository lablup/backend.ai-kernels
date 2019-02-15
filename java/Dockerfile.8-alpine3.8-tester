FROM alpine:3.8

RUN apk add --no-cache python3 && \
    ln -s /usr/bin/python3 /usr/bin/python
RUN python -m pip install -U pip setuptools && \
    python -m pip install --no-cache-dir tabulate dataclasses

# Install Java compile environments
# ref: https://github.com/docker-library/openjdk/blob/master/8-jdk/alpine/Dockerfile
# You may need to check the Alpine package repository for latest OpenJDK package available.
# ref: https://pkgs.alpinelinux.org/packages?name=openjdk8&branch=v3.8&repo=&arch=x86_64
RUN { \
        echo '#!/bin/sh'; \
        echo 'set -e'; \
        echo; \
        echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"'; \
    } > /usr/local/bin/docker-java-home \
    && chmod +x /usr/local/bin/docker-java-home
ENV JAVA_HOME /usr/lib/jvm/java-1.8-openjdk
ENV PATH $PATH:/usr/lib/jvm/java-1.8-openjdk/jre/bin:/usr/lib/jvm/java-1.8-openjdk/bin
ENV JAVA_VERSION 8u191
ENV JAVA_ALPINE_VERSION 8.191.12-r0

RUN set -x \
    && apk add --no-cache \
        openjdk8="$JAVA_ALPINE_VERSION" \
    && [ "$JAVA_HOME" = "$(docker-java-home)" ]

# Install Junit4 and some extensions.
RUN wget http://repo1.maven.org/maven2/junit/junit/4.12/junit-4.12.jar -P /usr/local/lib && \
    wget http://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar -P /usr/local/lib && \
    wget http://central.maven.org/maven2/com/github/stefanbirkner/system-rules/1.18.0/system-rules-1.18.0.jar -P /usr/local/lib && \
    wget http://repo1.maven.org/maven2/com/google/code/gson/gson/2.8.5/gson-2.8.5.jar -P /usr/local/lib

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="java" \
      ai.backend.runtime-path="/usr/lib/jvm/java-1.8-openjdk/bin/java" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
