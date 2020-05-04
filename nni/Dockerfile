FROM nvidia/cuda:9.0-cudnn7-runtime-ubuntu16.04

RUN DEBIAN_FRONTEND=noninteractive && \
    apt-get update -y && \
    apt-get -y install --no-install-recommends \
    apt-utils \
    git \
    curl \
    vim \
    unzip \
    wget \
    build-essential \
    cmake \
    libopenblas-dev \
    automake \
    openssh-client \
    openssh-server \
    lsof \
    python3.5 \
    python3-dev \
    python3-pip \
    python3-tk \
    libcupti-dev && \ 
    curl -sL https://deb.nodesource.com/setup_10.x | bash - && \
    apt-get update -y && \
    apt-get install -y nodejs && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# python alternative support
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2

RUN python3 -m pip install --upgrade pip setuptools==39.1.0 && \
    python3 -m pip --no-cache-dir install \
        numpy==1.14.3 \
	scipy==1.1.0 \
	&& \
    python3 -m pip --no-cache-dir install \
    	tensorflow-gpu==1.10.0 \
	Keras==2.1.6 \
	torch==1.4.0 \
	torchvision==0.5.0 \
	scikit-learn==0.20.0 \
	&& \
    python3 -m pip --no-cache-dir install pandas==0.23.4 lightgbm==2.2.2

# Jupyter notebook extension
RUN mkdir -p /home/work/.jupyter/nbextension
WORKDIR /home/work/.jupyter/nbextension

RUN python3 -m pip install --no-cache-dir imbalanced-learn && \
    python3 -m pip install --no-cache-dir scikit-optimize && \
    python3 -m pip install --no-cache-dir bayesian-optimization && \
    python3 -m pip install --no-cache-dir scikit-plot && \
    python3 -m pip install --no-cache-dir vecstack && \
    python3 -m pip install --no-cache-dir category_encoders && \
    python3 -m pip install --no-cache-dir scikit-surprise && \
    python3 -m pip install --no-cache-dir konlpy && \
    python3 -m pip install --no-cache-dir folium && \
    python3 -m pip install --no-cache-dir lime && \
    python3 -m pip install --no-cache-dir gensim && \
    python3 -m pip install --no-cache-dir pyldavis && \
    python3 -m pip install --no-cache-dir fbprophet && \
    python3 -m pip install --no-cache-dir seaborn && \
    python3 -m pip install --no-cache-dir bokeh && \
    python3 -m pip install --no-cache-dir plotly && \
    python3 -m pip install --no-cache-dir pygal && \
    python3 -m pip install --no-cache-dir mlxtend && \
#    python3 -m pip install --no-cache-dir dtreeviz && \
    python3 -m pip install --no-cache-dir pyglet && \
    python3 -m pip install --no-cache-dir descartes && \
    python3 -m pip install --no-cache-dir geoplotlib && \
    python3 -m pip install --no-cache-dir featuretools && \
    python3 -m pip install --no-cache-dir graphviz && \
    python3 -m pip install --no-cache-dir geopandas && \
    python3 -m pip install --no-cache-dir keras_applications && \
    python3 -m pip install --no-cache-dir keras_preprocessing && \
    python3 -m pip install --no-cache-dir pillow && \
    python3 -m pip install --no-cache-dir jupyter && \
    python3 -m pip install --no-cache-dir ipython && \
    python3 -m pip install --no-cache-dir ipywidgets && \
    python3 -m pip install --no-cache-dir ipyparallel && \
    python3 -m pip install --no-cache-dir jupyterlab && \
    python3 -m pip install --no-cache-dir jupyterthemes && \
    python3 -m pip install --no-cache-dir jupyter-js-widgets-nbextension && \
    python3 -m pip install --no-cache-dir jupyter_contrib_nbextensions && \
    python3 -m pip install --no-cache-dir jupyter_nbextensions_configurator && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir tensorwatch && \
    python3 -m pip install --no-cache-dir torchvision && \
    python3 -m pip install --no-cache-dir tensorflow-hub==0.5.0 && \
    python3 -m pip install --no-cache-dir tf2onnx && \
    python3 -m pip install --no-cache-dir contextlib2 && \
    python3 -m pip install --no-cache-dir pycocotools && \
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl

RUN jupyter nbextensions_configurator enable && \
    jupyter contrib nbextension install && \
    jupyter nbextension enable --py --sys-prefix widgetsnbextension && \
    jupyter contrib nbextension install && \
    jupyter serverextension enable --py jupyterlab --sys-prefix && \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager && \
    git clone https://github.com/lambdalisue/jupyter-vim-binding vim_binding && \
    jupyter nbextension enable /home/work/.jupyter/nbextension/vim_binding/vim_binding

COPY policy.yml /etc/backend.ai/jail/policy.yml

RUN python3 -m pip --no-cache-dir install nni

ENV PATH=/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/root/.local/bin:/usr/bin:/bin:/sbin

RUN python3 -m ipykernel install --display-name "Microsoft NNI Kernel 20.05 on Python 3.6 (CUDA9)" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,nniboard:http:8080,jupyter:http:8070,jupyterlab:http:8090"

WORKDIR /home/work
# vim: ft=dockerfile
