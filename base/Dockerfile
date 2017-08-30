FROM alpine:3.6
MAINTAINER DevOps "devops@lablup.com"

RUN apk add --no-cache libseccomp shadow su-exec; \
    mkdir /home/work && chmod 755 /home/work; \
    mkdir /home/sorna && chmod 755 /home/sorna
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# Install jail
COPY jail /home/sorna/jail
COPY patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001 2002 2003

LABEL io.sorna.timeout="30"
LABEL io.sorna.maxmem="128m"
LABEL io.sorna.maxcores="1"
LABEL io.sorna.version="2"
LABEL io.sorna.features="uid-match"

CMD ["/home/sorna/jail", "/bin/ash"]

# vim: sts=4 sw=4 et
