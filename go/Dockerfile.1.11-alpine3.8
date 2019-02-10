FROM golang:1.11-alpine3.8 as go-binary
FROM alpine:3.8

# Install Go environments
COPY --from=go-binary /usr/local/go /usr/local/

ENV GOPATH /home/work
ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" && chmod -R 777 "$GOPATH"
WORKDIR $GOPATH

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="golang" \
      ai.backend.runtime-path="/usr/local/bin/go" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
