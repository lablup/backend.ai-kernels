FROM ubuntu:18.04
# Ubuntu 18.04 comes with Python 3.6

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        python3 python3-pip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        gcc g++ && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8

RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir tabulate dataclasses && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "Python 3.6 on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080"
COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile
