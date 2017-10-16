FROM lablup/kernel-base-python3-minimal:latest

# Install minimal Rust compile environments
RUN apk add --no-cache build-base cargo rust

COPY run.py /home/sorna/run.py
COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "batch query uid-match"
