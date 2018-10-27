FROM ubuntu:16.04
MAINTAINER Mario Cho "m.cho@lablup.com"

# Install Intel MKL
# ref: https://software.intel.com/en-us/articles/installing-intel-free-libs-and-python-apt-repo

RUN apt update -y && \
    apt-get install -y --no-install-recommends ca-certificates apt-transport-https && \
    apt install -y wget && \
    wget -O intel-swprod.pub https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB && \
    apt-key add intel-swprod.pub && \
    wget https://apt.repos.intel.com/setup/intelproducts.list -O /etc/apt/sources.list.d/intelproducts.list && \
    echo deb https://apt.repos.intel.com/mkl all main > /etc/apt/sources.list.d/intel-mkl.list && \
    apt-get update -y && \
    apt-get install -y intel-mkl-2019.0-045 && \
    echo "/opt/intel/mkl/lib/intel64" > /etc/ld.so.conf.d/mkl.conf && \
    ldconfig && \
    rm -f intel-swprod.pub && \
    rm -rf /var/cache/apt/archives && \
    rm -rf /var/lib/apt/lists
