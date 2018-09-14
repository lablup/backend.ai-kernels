FROM alpine:3.8
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apk add --no-cache build-base git
WORKDIR /root
RUN git clone https://github.com/lablup/backend.ai-hook && \
    cd backend.ai-hook && \
    make inner

# vim: ft=dockerfile
