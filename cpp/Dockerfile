FROM lablup/kernel-base-python3-minimal:latest
MAINTAINER DevOps "devops@lablup.com"

# Install Python packages
RUN pip install --no-cache-dir aiozmq

# Install minimal C compile environments
RUN apk add --no-cache g++

COPY run.py /home/sorna/run.py

USER work
# TODO: With jail, `sh: setgid: Operation not permitted` error is raised.
# CMD ["/home/sorna/jail", "python3", "/usr/local/bin/python3", "/home/sorna/run.py"]
CMD ["/usr/local/bin/python3", "/home/sorna/run.py"]
