FROM lablup/kernel-base:jail as jail-builder
FROM lablup/kernel-base:hook as hook-builder
# ------------
FROM scratch
MAINTAINER Mario Cho "m.cho@lablup.com"

# The root file system are published at https://partner-images.canonical.com/core/ by Canonical
ADD ubuntu-bionic-core-cloudimg-amd64-root.tar.gz /
RUN set -xe && \
    echo '#!/bin/sh' > /usr/sbin/policy-rc.d && \
    echo 'exit 101' >> /usr/sbin/policy-rc.d && \
    chmod +x /usr/sbin/policy-rc.d && \
    \
    dpkg-divert --local --rename --add /sbin/initctl && \
    cp -a /usr/sbin/policy-rc.d /sbin/initctl && \
    sed -i 's/^exit.*/exit 0/' /sbin/initctl && \
    echo 'force-unsafe-io' > /etc/dpkg/dpkg.cfg.d/docker-apt-speedup

RUN echo 'DPkg::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' > /etc/apt/apt.conf.d/docker-clean && \
	echo 'APT::Update::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' >> /etc/apt/apt.conf.d/docker-clean && \
	echo 'Dir::Cache::pkgcache ""; Dir::Cache::srcpkgcache "";' >> /etc/apt/apt.conf.d/docker-clean

RUN echo 'DPkg::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' > /etc/apt/apt.conf.d/docker-clean

RUN echo 'Acquire::Languages "none";' > /etc/apt/apt.conf.d/docker-no-languages

RUN echo 'Acquire::GzipIndexes "true"; Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/docker-gzip-indexes

RUN echo 'Apt::AutoRemove::SuggestsImportant "false";' > /etc/apt/apt.conf.d/docker-autoremove-suggests


RUN rm -fr /var/lib/apt/lists/*
RUN sed -i 's/^#\s*\(deb.*universe\)$/\1/g' /etc/apt/sources.list
RUN mkdir -p /run/systemd && \
    echo 'docker' > /run/systemd/container

RUN export DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y && \
    apt-get install -y libseccomp2 gosu && \
    apt-get clean && \
    rm -r /var/lib/apt/lists /var/cache/apt/archives && \
    ln -s /usr/sbin/gosu /usr/sbin/su-exec && \
    mkdir /home/work && chmod 755 /home/work; \
    mkdir /home/backend.ai && chmod 755 /home/backend.ai
ADD entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# Install jail
COPY --from=jail-builder /go/src/github.com/lablup/backend.ai-jail/backend.ai-jail /home/backend.ai/jail
COPY --from=hook-builder /root/backend.ai-hook/libbaihook.so /home/backend.ai/libbaihook.so
ENV LD_PRELOAD /home/backend.ai/libbaihook.so

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL ai.backend.version="2" \
      ai.backend.features="uid-match"

CMD ["/home/backend.ai/jail", "/bin/bash"]

# vim: ft=dockerfile
