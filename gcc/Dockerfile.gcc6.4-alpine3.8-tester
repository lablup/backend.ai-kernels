FROM alpine:3.8

# Install minimal C compile environments
RUN apk add --no-cache build-base python3 && \
    ln -s /usr/bin/python3 /usr/bin/python
RUN python -m pip install -U pip setuptools && \
    python -m pip install --no-cache-dir tabulate dataclasses

# Install Google Test and Google Mock.
# ENV GTEST_MASTER_DIR=/googletest-master
# ENV GTEST_DIR=${GTEST_MASTER_DIR}/googletest
# ENV GMOCK_DIR=${GTEST_MASTER_DIR}/googlemock
RUN wget https://github.com/google/googletest/archive/master.zip && \
    unzip master.zip && mkdir -p /usr/local/include && \
    export GTEST_MASTER_DIR=/googletest-master && \
    export GTEST_DIR=${GTEST_MASTER_DIR}/googletest && \
    export GMOCK_DIR=${GTEST_MASTER_DIR}/googlemock && \
    cd ${GTEST_DIR} && mkdir mybuild && cd mybuild && \
    g++ -isystem ${GTEST_DIR}/include -I${GTEST_DIR} \
        -isystem ${GMOCK_DIR}/include -I${GMOCK_DIR} \
        -pthread -c ${GTEST_DIR}/src/gtest-all.cc && \
    ar -rv libgtest.a gtest-all.o && \
    cp *.a /usr/local/lib && cp -r ${GTEST_DIR}/include/gtest /usr/local/include && \
    cd ${GMOCK_DIR} && mkdir mybuild && cd mybuild && \
    g++ -isystem ${GTEST_DIR}/include -I${GTEST_DIR} \
        -isystem ${GMOCK_DIR}/include -I${GMOCK_DIR} \
        -pthread -c ${GMOCK_DIR}/src/gmock-all.cc && \
    ar -rv libgmock.a gmock-all.o && \
    cp *.a /usr/local/lib && cp -r ${GMOCK_DIR}/include/gmock /usr/local/include && \
    rm -rf /master.zip && rm -rf /googletest-master

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.runtime-type="c" \
      ai.backend.runtime-path="/usr/bin/gcc" \
      ai.backend.service-ports=""

# vim: ft=dockerfile
