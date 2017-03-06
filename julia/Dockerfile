# Base on full-fledged Python 3 kernel to enable access to science packages in
# Julia via PyCall.

# NOTE: Use --squash option before publish the image.

FROM lablup/kernel-python3:latest

# Install Julia
# reference: https://github.com/docker-library/julia/blob/master/Dockerfile

# Check Julia dependencies: https://pkgs.alpinelinux.org/package/edge/testing/x86_64/julia

ENV JULIA_PATH /usr/local/julia
ENV JULIA_VERSION 0.5.0
ENV JULIA_VERSION_SHORT 0.5
ENV JULIA_PKGDIR "/usr/local/share/julia/site"

USER root
RUN mkdir -p $JULIA_PATH

COPY Make.user /tmp/
COPY sysctl-fix.patch /tmp/
COPY julia-*.tar.gz /usr/local/julia/

RUN mkdir -p /usr/local/share/julia/site/ \
    && chown work:work /usr/local/share/julia/site/ \
    && apk add --no-cache --virtual .sorna-deps curl \
    && apk add --no-cache --virtual .build-deps \
        gnupg git tar \
    && cd $JULIA_PATH \
    # We store the full source file locally due to slow networks...
    && curl -L "https://github.com/JuliaLang/julia/releases/download/v${JULIA_VERSION}/julia-${JULIA_VERSION}-full.tar.gz.asc" -o sign.asc \
    && export GNUPGHOME="$(mktemp -d)" \
    # http://julialang.org/juliareleases.asc
    # Julia (Binary signing key) <buildbot@julialang.org>
    && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys 3673DF529D9049477F76B37566E3C7DC03D6E495 \
    && gpg --batch --verify sign.asc julia-${JULIA_VERSION}-full.tar.gz \
    && rm -r "$GNUPGHOME" sign.asc \
    # Decompress the source and build!
    && tar -xzf julia-${JULIA_VERSION}-full.tar.gz --strip-components=1 -C . \
    && apk add --no-cache --virtual .sorna-deps \
        libgcc libc-dev llvm-libs m4 pcre2 patchelf \
        gmp fftw arpack mpfr3 dsfmt openblas openlibm openspecfun \
        libgit2 libssh2 libressl libressl2.4-libcrypto libressl2.4-libssl libressl2.4-libtls \
        libunwind libexecinfo suitesparse utf8proc \
    && apk add --no-cache --virtual .build-deps \
        build-base llvm-dev pcre2-dev linux-headers gfortran perl \
        gmp-dev fftw-dev arpack-dev dsfmt-dev mpfr-dev openblas-dev openlibm-dev \
        openspecfun-dev \
        libgit2-dev libssh2-dev libressl-dev \
        libunwind-dev libexecinfo-dev suitesparse-dev utf8proc-dev \
    && mv /tmp/Make.user . \
    && patch -p0 < /tmp/sysctl-fix.patch \
    && make -j8 \
    && apk del .build-deps \
    && rm -rf julia-*.tar.gz deps

ENV PATH /usr/local/julia:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

COPY package.jl /home/sorna/package.jl
COPY precompile.jl /home/sorna/precompile.jl

RUN apk add --no-cache --virtual .sorna-deps \
        libgit2-dev pcre2-dev mpfr-dev gmp-dev \
    && apk add --no-cache --virtual .build-deps \
        build-base libc-dev linux-headers pkgconf \
    && julia /home/sorna/package.jl \
    && julia /home/sorna/precompile.jl \
    && chown -R work:work $JULIA_PKGDIR \
    && apk del .build-deps
USER work

# Apply higher resource limits
LABEL io.sorna.timeout="180"
LABEL io.sorna.maxmem="512m"
LABEL io.sorna.maxcores="4"
LABEL io.sorna.mode="query"
LABEL io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,JULIA_CPU_CORES"
LABEL io.sorna.version="2"

COPY run.jl /home/sorna/run.jl

CMD ["/home/sorna/jail", "julia", "/usr/local/julia/julia", "/home/sorna/run.jl"]

# vim: ts=8 sts=4 sw=4 et
