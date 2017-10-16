FROM golang:1.9.0-alpine3.6 as go-binary


FROM lablup/kernel-base-python3-minimal:latest

# Install Go environments
COPY --from=go-binary /usr/local/go /usr/local/
RUN apk add --no-cache ca-certificates

ENV GOPATH /home/work
ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" && chmod -R 777 "$GOPATH"
WORKDIR $GOPATH

COPY run.py /home/sorna/
COPY policy.yml /home/sorna/

LABEL io.sorna.features "batch query uid-match user-input"
