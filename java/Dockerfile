FROM lablup/kernel-base-python3-minimal:latest

# Install Java compile environments
# ref: https://github.com/docker-library/openjdk/blob/master/8-jdk/alpine/Dockerfile
# You may need to check the Alpine package repository for latest OpenJDK package available.
# ref: https://pkgs.alpinelinux.org/packages?name=openjdk8&branch=v3.6&repo=&arch=x86_64
ENV JAVA_HOME /usr/lib/jvm/java-1.8-openjdk
ENV PATH $PATH:/usr/lib/jvm/java-1.8-openjdk/jre/bin:/usr/lib/jvm/java-1.8-openjdk/bin
ENV JAVA_VERSION 8u131
ENV JAVA_ALPINE_VERSION 8.131.11-r2
RUN { \
		echo '#!/bin/sh'; \
		echo 'set -e'; \
		echo; \
		echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"'; \
	} > /usr/local/bin/docker-java-home \
	&& chmod +x /usr/local/bin/docker-java-home
RUN set -x \
	&& apk add --no-cache \
		openjdk8="$JAVA_ALPINE_VERSION" \
	&& [ "$JAVA_HOME" = "$(docker-java-home)" ]

COPY run.py /home/sorna/run.py
COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "batch query uid-match"
