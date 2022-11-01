FROM lablup/python-tensorflow:2.4-py38-cuda11.1

USER root
ENV DEBIAN_FRONTEND=noninteractive

# Update the NVIDIA GPG signing key.
RUN apt-key del 7fa2af80 && \
    apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/3bf863cc.pub && \
    rm -f /etc/apt/sources.list.d/cuda.list && \
    rm -f /etc/apt/sources.list.d/nvidia-ml.list

RUN apt update -y && \
    apt install -y ncurses-term gnupg2 && \
    ln -sf /usr/share/terminfo/x/xterm-color /usr/share/terminfo/x/xterm-256color

# Jupyter Notebook
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - && \
    apt update -y && \
    apt install -y nodejs
RUN pip install --no-cache-dir jupyterlab jupyter_contrib_nbextensions jupyterthemes && \
    jupyter nbextensions_configurator enable && \
    jupyter contrib nbextension install && \
    jupyter lab build

# Code-Server
RUN CODESERVER_VERSION=4.7.1 && \
    curl -fL https://github.com/cdr/code-server/releases/download/v${CODESERVER_VERSION}/code-server-${CODESERVER_VERSION}-linux-amd64.tar.gz | tar -C /usr/local/lib -xz && \
    mv /usr/local/lib/code-server-${CODESERVER_VERSION}-linux-amd64 /usr/local/lib/code-server-${CODESERVER_VERSION} && \
    ln -sf /usr/local/lib/code-server-${CODESERVER_VERSION}/bin/code-server /usr/local/bin/code-server

# Additional deb packages
RUN apt install -y liblapacke liblapacke-dev

# Additional PyPI packages
RUN python3 -m pip uninstall -y enum34 typing && \
    python3 -m pip install -U Cython==0.29.32 && \
    python3 -m pip install -U tensorflow==2.4.4 tensorflow-probability==0.12.1 tensorflow-transform==0.30.0 tfx==0.30.0 && \
    python3 -m pip install -U cloudpickle==1.5.0 tf-agents==0.7.1 pybind11 holoviews==1.12.7

# AIMET
RUN export AIMET_VARIANT=tf_gpu && \
    export release_tag=1.22.2 && \
    export download_url="https://github.com/quic/aimet/releases/download/${release_tag}" && \
    export wheel_file_suffix="cp38-cp38-linux_x86_64.whl" && \
    python3 -m pip install ${download_url}/AimetCommon-${AIMET_VARIANT}_${release_tag}-${wheel_file_suffix} && \
    python3 -m pip install ${download_url}/AimetTensorflow-${AIMET_VARIANT}_${release_tag}-${wheel_file_suffix} && \
    python3 -m pip install ${download_url}/Aimet-${AIMET_VARIANT}_${release_tag}-${wheel_file_suffix} && \
    python3 -m pip uninstall -y pillow && \
    python3 -m pip install --no-cache-dir Pillow-SIMD==7.0.0.post3 && \
    ln -s /usr/lib/x86_64-linux-gnu/libjpeg.so /usr/lib && \
    ln -s /usr/local/cuda-11.0 /usr/local/cuda
# RUN cat /usr/local/lib/python3.8/dist-packages/aimet_common/bin/reqs_deb_common.txt | xargs apt-get --assume-yes install && \
#     cat /usr/local/lib/python3.8/dist-packages/aimet_tensorflow/bin/reqs_deb_tf_gpu.txt | xargs apt-get --assume-yes install

# Cleanup
RUN apt clean && rm -rf /var/lib/apt/lists/ && rm -rf /root/.cache && rm -rf /tmp/* && \
    rm -rf /root/.cache && rm -f /tmp/*.whl

LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.accelerators="cuda" \
      ai.backend.features="uid-match" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.preferred.shmem="64" \
      ai.backend.resource.min.cuda.device=0 \
      ai.backend.resource.min.cuda.shares=0 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python" \
      ai.backend.service-ports="jupyter:http:8080,jupyterlab:http:8090,vscode:http:8180"

COPY policy.yml /etc/backend.ai/jail/policy.yml
COPY ./service-defs /etc/backend.ai/service-defs
# COPY bootstrap.sh /opt/container/bootstrap.sh

ENV LD_LIBRARY_PATH=/usr/local/lib/python3.8/dist-packages/aimet_common:$LD_LIBRARY_PATH
ENV PATH=/usr/local/nvidia/bin:/usr/local/cuda/bin:$PATH
ENV LD_LIBRARY_PATH=/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/local/cuda/targets/x86_64-linux/lib:$LD_LIBRARY_PATH
ENV CUDA_TOOLKIT_PATH=/usr/local/cuda
ENV CUDNN_INSTALL_PATH=/usr/local/cuda
ENV CUDA_HOME=/usr/local/cuda
ENV NVIDIA_DRIVER_CAPABILITIES=compute,utility
ENV NVIDIA_VISIBLE_DEVICES=all

WORKDIR /home/work
# vim: ft=dockerfile
