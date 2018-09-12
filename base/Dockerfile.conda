FROM lablup/kernel-base:jail as jail-builder
FROM lablup/kernel-base:hook as hook-builder
# ------------
FROM bitnami/minideb:stretch
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN echo "deb http://ftp.debian.org/debian stretch-backports main" > /etc/apt/sources.list.d/backports.list && \
    export DEBIAN_FRONTEND=noninteractive && \
    apt update && apt -t stretch-backports install -y libseccomp2 gosu wget bzip2 && \
    apt clean && \
    rm -r /var/lib/apt/lists /var/cache/apt/archives && \
    ln -s /usr/sbin/gosu /usr/sbin/su-exec && \
    mkdir /home/work && chmod 755 /home/work && \
    mkdir /home/backend.ai && chmod 755 /home/backend.ai
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# Install jail
COPY --from=jail-builder /go/src/github.com/lablup/backend.ai-jail/backend.ai-jail /home/backend.ai/jail
COPY --from=hook-builder /root/backend.ai-hook/libbaihook.so /home/backend.ai/libbaihook.so
ENV LD_PRELOAD /home/backend.ai/libbaihook.so


RUN wget --quiet https://repo.continuum.io/miniconda/Miniconda3-4.5.4-Linux-x86_64.sh \
         -O ~/miniconda.sh && \
    /bin/bash ~/miniconda.sh -b -p /opt/conda && \
    rm ~/miniconda.sh && \
    /opt/conda/bin/conda clean -tipsy && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc

ENV TINI_VERSION v0.16.1
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /usr/bin/tini
RUN chmod +x /usr/bin/tini

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL ai.backend.version="2" \
      ai.backend.features="uid-match"

CMD ["/home/backend.ai/jail", "/bin/bash"]

# vim: ft=dockerfile
