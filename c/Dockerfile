FROM lablup/kernel-base-python3-minimal:latest
MAINTAINER DevOps "devops@lablup.com"

# Install minimal C compile environments
RUN apk add --no-cache gcc musl-dev

COPY run.py /home/sorna/run.py
COPY policy.yml /home/sorna/policy.yml
#COPY patch-libs.so /home/sorna/patch-libs.so
#COPY base_run.py /home/sorna/base_run.py

# Test using runner script from kernel-runner package
RUN apk add --no-cache git
RUN cd /home/sorna && \
    git clone https://github.com/lablup/backend.ai-kernel-runner.git
RUN cd /home/sorna/backend.ai-kernel-runner && pip install -e .
CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "c"]

LABEL io.sorna.features "batch query uid-match user-input"

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", "/usr/local/bin/python3", "/home/sorna/run.py"]
