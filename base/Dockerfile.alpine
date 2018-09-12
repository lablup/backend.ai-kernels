FROM lablup/kernel-base:jail-alpine as jail-builder
FROM lablup/kernel-base:hook-alpine as hook-builder
# ------------
FROM alpine:3.8
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apk add --no-cache libseccomp shadow su-exec libstdc++; \
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

CMD ["/home/backend.ai/jail", "/bin/ash"]

# vim: ft=dockerfile
