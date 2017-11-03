FROM lablup/kernel-base-python3-minimal:latest

# Install minimal Rust compile environments
RUN apk add --no-cache build-base cargo rust

# Install kernel-runner scripts package
RUN pip install --no-cache-dir "backend.ai-kernel-runner[rust]~=1.0.2"

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "batch query uid-match"

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "rust"]
