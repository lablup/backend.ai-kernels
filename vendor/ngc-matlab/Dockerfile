FROM nvcr.io/partners/matlab:r2019b

ENV PYTHONUNBUFFERED=1 \
    LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib:/usr/local/nvidia/lib64" \
    PATH=/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH \
    LIBRARY_PATH="/usr/local/cuda/lib64/stubs" \
    LANG=C.UTF-8

# Backend.AI specifics
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch uid-match" \
      ai.backend.accelerators="cuda" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.preferred.shmem="512m" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="matlab" \
      ai.backend.runtime-path="/usr/local/bin/matlab" \
      ai.backend.service-ports="vnc:tcp:5901,matlab:http:6080"

WORKDIR /home/work
# vim: ft=dockerfile
