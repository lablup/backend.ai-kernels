FROM alpine:3.6
MAINTAINER DevOps "devops@lablup.com"

# Add an isolated user
# /home/work: actual working directory for user codes
# /home/sorna: place for Python and REPL script
RUN adduser -D -g "" work \
    && chmod 700 /home/work \
    && mkdir /home/sorna \
    && chmod 755 /home/sorna \
    && chown -R work:work /home/sorna

# Install jail
RUN apk add --no-cache libseccomp shadow
COPY sorna-jail /home/sorna/sorna-jail
COPY patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD /home/sorna/patch-libs.so

# Common files
COPY base_run.py /home/sorna/base_run.py

ENV HOME /home/work
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000 2001

LABEL io.sorna.timeout="30"
LABEL io.sorna.maxmem="128m"
LABEL io.sorna.maxcores="1"
LABEL io.sorna.version="2"

CMD ["/home/sorna/jail", "default", "/bin/ash"]

# vim: sts=4 sw=4 et
