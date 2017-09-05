FROM lablup/kernel-base-python3-minimal:latest

# Install minimal C++ compile environments
RUN apk add --no-cache build-base libstdc++

COPY run.py /home/sorna/run.py
COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "batch query uid-match"
