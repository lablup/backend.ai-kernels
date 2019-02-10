FROM alpine:3.8

# Install minimal C++ compile environments
RUN apk add --no-cache openssl build-base libstdc++ \
    && apk add --no-cache curl wget zip unzip vim

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="cpp" \
      ai.backend.runtime-path="/usr/bin/g++" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
