FROM lablup/kernel-base-python3-minimal:latest

# Install minimal C compile environments
RUN apk add --no-cache build-base

# Install kernel-runner scripts package
# TODO: use PyPI's package if available
RUN apk add --no-cache git
RUN cd /home/sorna && \
    git clone https://github.com/lablup/backend.ai-kernel-runner.git
RUN cd /home/sorna/backend.ai-kernel-runner && pip install -e .[c]
# RUN pip install backend.ai-kernel-runner[c]

COPY policy.yml /home/sorna/policy.yml

LABEL io.sorna.features "batch query uid-match user-input"

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "c"]
