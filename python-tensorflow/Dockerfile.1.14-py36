FROM lablup/common-tensorflow:1.14-py36 as tf-binary
FROM lablup/common-base:19.06-py36

COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp
    
# Jupyter notebook extension 
RUN mkdir -p /home/work/.jupyter/nbextension
WORKDIR /home/work/.jupyter/nbextension

RUN python3 -m pip install --no-cache-dir opencv-python

RUN python3 -m pip install --no-cache-dir wheel /tmp/*.whl && \
    python3 -m pip install --no-cache-dir pystan && \
    python3 -m pip install --no-cache-dir fbprophet && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir keras && \
    python3 -m pip install --no-cache-dir keras_applications && \
    python3 -m pip install --no-cache-dir keras_preprocessing && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir ipywidgets && \
    python3 -m pip install --no-cache-dir ipyparallel && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    python3 -m pip install --no-cache-dir jupyterthemes && \    
    python3 -m pip install --no-cache-dir jupyter-js-widgets-nbextension && \
    python3 -m pip install --no-cache-dir jupyter_contrib_nbextensions && \
    python3 -m pip install --no-cache-dir jupyter_nbextensions_configurator && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir tf2onnx && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

# python alternative support
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

RUN jupyter nbextensions_configurator enable && \
    jupyter contrib nbextension install && \
    jupyter nbextension enable --py --sys-prefix widgetsnbextension && \
    jupyter contrib nbextension install && \
    jupyter serverextension enable --py jupyterlab --sys-prefix && \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager && \
    git clone https://github.com/lambdalisue/jupyter-vim-binding vim_binding && \
    jupyter nbextension enable /home/work/.jupyter/nbextension/vim_binding/vim_binding

# Install ipython kernelspec
RUN python3 -m ipykernel install --display-name "TensorFlow 1.14 on Python 3.6 (CPU-only)" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# for apt-get installation using /tmp
RUN mkdir -p /tmp && \
    chown root:root /tmp && \
    chmod 1777 /tmp
    
# Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu18.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

COPY policy.yml /etc/backend.ai/jail/policy.yml
WORKDIR /home/work
# vim: ft=dockerfile