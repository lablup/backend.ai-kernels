FROM lablup/kernel-base-python3-minimal:latest
MAINTAINER DevOps "devops@lablup.com"

# Install minimal C compile environments
RUN apk add --no-cache gcc musl-dev

# Install Python packages
RUN pip install --no-cache-dir aiozmq

COPY run.py /home/sorna/run.py
COPY policy.yml /home/sorna/policy.yml
COPY patch-libs.so /home/sorna/patch-libs.so

USER work
CMD ["/home/sorna/sorna-jail", "-policy", "/home/sorna/policy.yml", "/usr/local/bin/python3", "/home/sorna/run.py"]
