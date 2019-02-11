FROM ubuntu:18.04

# Install Octave
# RUN apt-get install -y liboctave-dev info
RUN apt-get update && \
    apt-get install -y \
        octave \
        ghostscript epstool transfig pstoedit gnuplot

# Install octave packages
ADD packages.m /home/work/packages.m
RUN octave-cli /home/work/packages.m

LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="128m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="octave" \
      ai.backend.runtime-path="/usr/bin/octave-cli" \
      ai.backend.service-ports=""
COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile
