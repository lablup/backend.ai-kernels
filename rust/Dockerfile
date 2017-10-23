FROM lablup/kernel-base-python3-minimal:latest

# Install minimal Rust compile environments
RUN apk add --no-cache build-base cargo rust

# Install kernel-runner scripts package
# TODO: use PyPI's package if available
RUN apk add --no-cache git
RUN cd /home/sorna && \
    git clone https://github.com/lablup/backend.ai-kernel-runner.git
RUN cd /home/sorna/backend.ai-kernel-runner && pip install -e .[rust]
# RUN pip install backend.ai-kernel-runner[rust]

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "batch query uid-match"

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "rust"]
