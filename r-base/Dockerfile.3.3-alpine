FROM lablup/kernel-base-python-minimal:3.6-alpine
MAINTAINER Mario Cho "m.cho@lablup.com"

# Install dependencies for R package installation
RUN apk update && \
    apk add --no-cache gcc g++ make libxml2-dev ncurses

# Install R and its packages
RUN apk add --no-cache R R-dev

ADD install-packages.R /home/backend.ai/install-packages.R
RUN Rscript /home/backend.ai/install-packages.R

# Install kernel-runner scripts package
RUN pip install --no-cache-dir "backend.ai-kernel-runner[r]~=1.4.0"

LABEL ai.backend.features "query uid-match"

CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "r"]
