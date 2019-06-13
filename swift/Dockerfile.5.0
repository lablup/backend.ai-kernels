# Apple Swfit 5.0 for Backend.ai

FROM ubuntu:18.04
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true && \
    apt-get -q update -y && \
    apt-get -q install -y \
        python3 python3-pip \
        libssl-dev \	
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        libatomic1 \
	libbsd0 \
	libcurl4 \
	libxml2 \
	libedit2 \
	libsqlite3-0 \
	libc6-dev \
	binutils \
	libgcc-5-dev \
	libstdc++-5-dev \
	libpython2.7 \
	tzdata \
	git \
	pkg-config \
	&& \
    rm -r /var/lib/apt/lists/*

ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8

ARG SWIFT_PLATFORM=ubuntu18.04
ARG SWIFT_BRANCH=swift-5.0.1-release
ARG SWIFT_VERSION=swift-5.0.1-RELEASE

ENV SWIFT_PLATFORM=$SWIFT_PLATFORM \
    SWIFT_BRANCH=$SWIFT_BRANCH \
    SWIFT_VERSION=$SWIFT_VERSION

RUN SWIFT_URL=https://swift.org/builds/$SWIFT_BRANCH/$(echo "$SWIFT_PLATFORM" | tr -d .)/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz && \
    apt-get update && \
    apt-get install -y curl && \
    curl -fSsL $SWIFT_URL -o swift.tar.gz && \
    curl -fSsL $SWIFT_URL.sig -o swift.tar.gz.sig && \
    export GNUPGHOME="$(mktemp -d)" && \
    set -e; \
        for key in \
      # pub   4096R/ED3D1561 2019-03-22 [expires: 2021-03-21]
      #       Key fingerprint = A62A E125 BBBF BB96 A6E0  42EC 925C C1CC ED3D 1561
      # uid                  Swift 5.x Release Signing Key <swift-infrastructure@swift.org          
          A62AE125BBBFBB96A6E042EC925CC1CCED3D1561 \
        ; do \
          gpg --quiet --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
        done && \
    gpg --batch --verify --quiet swift.tar.gz.sig swift.tar.gz && \
    tar -xzf swift.tar.gz --directory / --strip-components=1 && \
    rm -r "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz && \
    chmod -R o+r /usr/lib/swift

RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools && \
    python3 -m pip install --no-cache-dir wheel && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir numpy scipy && \
    python3 -m pip install --no-cache-dir versioneer==0.17 && \
    python3 -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    apt-get purge -y curl && \
    apt-get -y autoremove && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl && \
    swift --version

RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "Swift 5.0" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/swift" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile