FROM nvcr.io/nvidia/pytorch:18.12.1-py3

# Prepare for jail/hook minimal dependencies
RUN apt-get update && \
    apt-get install -y libseccomp2 libstdc++5 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

# Install ipython/jupyter
RUN /opt/conda/bin/pip install --no-cache-dir ipython jupyter

# Prepare runtime environment
ENV PYTHONUNBUFFERED=1 \
    PATH=/opt/backend.ai/bin:/opt/conda/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LANG=C.UTF-8
WORKDIR /home/work
EXPOSE 2000 2001

LABEL ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch uid-match" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080"

ENTRYPOINT []
CMD ["/opt/backend.ai/bin/entrypoint.sh", \
     "/opt/backend.ai/bin/jail", "-policy", "/opt/backend.ai/etc/jail/policy.yml", \
     "/opt/backend.ai/bin/python", "-m", "ai.backend.kernel", "ngc_pytorch"]
