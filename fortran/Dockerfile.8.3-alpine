FROM lablup/kernel-base-python-minimal:3.6-alpine
MAINTAINER Mario Cho "m.cho@lablup.com"

# Install pre-build matplotlib
RUN apt install build-base \
    && pip install --no-cache-dir matplotlib bokeh \
    && pip install --no-cache-dir ipython \
    && pip install --no-cache-dir pandas \
    && apt-get remove --purge -y gcc g++ \
    && apt-get autoremove -y \
    && rm -f /tmp/*.whl

# Install Julia
# reference: https://github.com/docker-library/julia/blob/master/Dockerfile
# Check Julia dependencies: https://pkgs.alpinelinux.org/package/edge/testing/x86_64/julia
ARG JULIA_VERSION

ENV JULIA_VERSION ${JULIA_VERSION:-0.7.0}
ENV JULIA_PATH "/usr/local/julia"
ENV JULIA_PKGDIR "/usr/local/share/julia/site"
ENV JULIA_GPG 3673DF529D9049477F76B37566E3C7DC03D6E495
ENV PATH $JULIA_PATH:$JULIA_PATH/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

COPY Make.user /tmp/

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*

RUN set -ex; \
    dpkgArch="$(dpkg --print-architecture)"; \
    case "${dpkgArch##*-}" in \
        amd64) tarArch='x86_64'; dirArch='x64'; sha256='3a27ea78b06f46701dc4274820d9853789db205bce56afdc7147f7bd6fa83e41' ;; \
        armhf) tarArch='arm'; dirArch='arm'; sha256='7515f5977b2aac0cea1333ef249b3983928dee76ea8eb3de9dd6a7cdfbd07d2d' ;; \
        i386) tarArch='i686'; dirArch='x86'; sha256='bfebd2ef38c25ce72dd6661cdd8a6f509800492a4d250c2908f83e791c0a444a' ;; \
        *) echo >&2 "error: current architecture ($dpkgArch) does not have a corresponding Julia binary release"; exit 1 ;; \
    esac; \
    \
    curl -fL -o julia.tar.gz     "https://julialang-s3.julialang.org/bin/linux/${dirArch}/${JULIA_VERSION%[.-]*}/julia-${JULIA_VERSION}-linux-${tarArch}.tar.gz"; \
    curl -fL -o julia.tar.gz.asc "https://julialang-s3.julialang.org/bin/linux/${dirArch}/${JULIA_VERSION%[.-]*}/julia-${JULIA_VERSION}-linux-${tarArch}.tar.gz.asc"; \
    \
    echo "${sha256} *julia.tar.gz" | sha256sum -c -; \
    \
    export GNUPGHOME="$(mktemp -d)"; \
    gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$JULIA_GPG"; \
    gpg --batch --verify julia.tar.gz.asc julia.tar.gz; \
    rm -rf "$GNUPGHOME" julia.tar.gz.asc; \
    \
    mkdir "$JULIA_PATH"; \
    tar -xzf julia.tar.gz -C "$JULIA_PATH" --strip-components 1; \
    rm julia.tar.gz

# Install kernel-runner scripts package
RUN pip install --no-cache-dir "backend.ai-kernel-runner[julia]~=1.4.0"

# Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR /home/backend.ai/.matplotlib
RUN mkdir /home/backend.ai/.matplotlib
COPY matplotlibrc /home/backend.ai/.matplotlib/
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
    && python /tmp/matplotlib-fontcache.py \
    && rm /tmp/matplotlib-fontcache.py

# Install / pre-compile julia packages
# matplotlib is needed for PyPlot
COPY IJuliaEmul.jl /home/backend.ai/IJuliaEmul.jl
COPY package.jl /home/backend.ai/package.jl
COPY precompile.jl /home/backend.ai/precompile.jl
RUN julia /home/backend.ai/package.jl
RUN julia /home/backend.ai/precompile.jl

# Apply higher resource limits
LABEL ai.backend.timeout="180"
LABEL ai.backend.maxmem="512m"
LABEL ai.backend.maxcores="4"
LABEL ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,JULIA_CPU_CORES"
LABEL ai.backend.features "query uid-match"

CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "julia"]

# vim: ft=dockerfile
