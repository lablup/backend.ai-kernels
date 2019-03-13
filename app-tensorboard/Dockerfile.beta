FROM lablup/kernel-python-tensorflow:1.13-py36
RUN pip install --no-cache-dir tensorboard
RUN pip install --no-cache-dir aiohttp
COPY run.sh /home/backend.ai/run.sh
COPY pserver.py /home/backend.ai/pserver.py

LABEL ai.backend.timeout="0" \
      ai.backend.maxmem="8g" \
      ai.backend.maxcores="4" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input"

CMD "/home/backend.ai/run.sh"

# vim: ft=dockerfile
