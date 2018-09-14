FROM debian:stretch-slim
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt update -y && \    
    apt install -y gcc g++ libc6-dev make git-core ca-certificates
WORKDIR /root
RUN git clone https://github.com/lablup/backend.ai-hook && \
    cd backend.ai-hook && \
    make inner

# vim: ft=dockerfile
