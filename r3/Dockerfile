FROM lablup/kernel-base-python3-minimal:latest

# Install dependencies for R package installation
RUN apk update && apk add --no-cache gcc g++ make

# Install R and its packages
RUN apk add --no-cache R R-dev
ADD install-packages.R /home/sorna/install-packages.R
RUN Rscript /home/sorna/install-packages.R

LABEL io.sorna.features="query uid-match"

# Install kernel-runner scripts package
# TODO: use PyPI's package if available
RUN apk add --no-cache git
RUN cd /home/sorna && \
    git clone https://github.com/lablup/backend.ai-kernel-runner.git
RUN cd /home/sorna/backend.ai-kernel-runner && pip install -e .[r]
# RUN pip install backend.ai-kernel-runner[r]

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "r"]
