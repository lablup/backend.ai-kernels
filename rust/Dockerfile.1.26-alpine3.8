FROM alpine:3.8

# Install minimal Rust compile environments
RUN apk add --no-cache build-base cargo rust

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="rust" \
      ai.backend.runtime-path="/usr/bin/rustc" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
