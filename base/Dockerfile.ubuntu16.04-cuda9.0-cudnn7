FROM lablup/kernel-base:jail as jail-builder
FROM lablup/kernel-base:hook as hook-builder
# ------------
FROM nvidia/cuda:9.0-cudnn7-runtime-ubuntu16.04
MAINTAINER DevOps "devops@lablup.com"
ENV DEBIAN_FRONTEND=noninteractive
RUN sed -i.bak -e \
      "s%http://archive.ubuntu.com/%http://ftp.daumkakao.com/%g" \
      /etc/apt/sources.list; \
    sed -i.bak -e \
      "s%http://security.ubuntu.com/%http://ftp.daumkakao.com/%g" \
      /etc/apt/sources.list; \
    apt-get update

RUN apt-get install -y libseccomp2 gosu && \
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

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL ai.backend.version="2" \
      ai.backend.features="uid-match"

CMD ["/home/backend.ai/jail", "/bin/bash"]

# vim: ft=dockerfile
