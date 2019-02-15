FROM alpine:3.8

RUN apk add --no-cache python3 && \
    ln -s /usr/bin/python3 /usr/bin/python
RUN python -m pip install -U pip setuptools && \
    python -m pip install --no-cache-dir tabulate dataclasses

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
