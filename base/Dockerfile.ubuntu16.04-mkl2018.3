FROM lablup/kernel-base:jail as jail-builder
FROM lablup/kernel-base:hook as hook-builder
# ------------
FROM ubuntu:16.04
MAINTAINER Mario Cho "m.cho@lablup.com"

# Install Intel MKL
# ref: https://software.intel.com/en-us/articles/installing-intel-free-libs-and-python-apt-repo
RUN apt update -y && \
    apt-get install -y --no-install-recommends ca-certificates apt-transport-https && \
    apt install -y wget && \
    wget -O intel-swprod.pub https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB && \
    apt-key add intel-swprod.pub && \
    echo deb https://apt.repos.intel.com/mkl all main > /etc/apt/sources.list.d/intel-mkl.list && \
    apt-get update && \
    apt-get install -y intel-mkl-2018.3-051 && \
    echo "/opt/intel/mkl/lib/intel64" > /etc/ld.so.conf.d/mkl.conf && \
    ldconfig && \
    rm -f intel-swprod.pub && \
    rm -rf /var/cache/apt/archives && \
    rm -rf /var/lib/apt/lists

RUN apt-get install -y libseccomp2 gosu && \
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

LABEL ai.backend.timeout="30" \
      ai.backend.maxmem="128m" \
      ai.backend.maxcores="1" \
      ai.backend.version="2" \
      ai.backend.features="uid-match"

CMD ["/home/backend.ai/jail", "/bin/bash"]

# vim: ft=dockerfile
