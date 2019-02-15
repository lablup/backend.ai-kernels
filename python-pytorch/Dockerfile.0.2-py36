FROM lablup/python:3.6-ubuntu18.04

# Install Pytorch
RUN python -m pip install --no-cache-dir \
        http://download.pytorch.org/whl/cu80/torch-0.2.0.post3-cp36-cp36m-manylinux1_x86_64.whl \
        torchvision && \
    rm -rf /root/.cache

# Install ipython kernelspec
RUN python -m ipykernel install --display-name "PyTorch 0.2 on Python 3.6 (CPU-only)" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"
COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile sts=4 sw=4 et tw=0
