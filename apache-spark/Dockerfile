FROM ubuntu:18.04
# Ubuntu 18.04 comes with Python 3.6

RUN apt-get update && \
    apt-get install -y \
        ca-certificates \
        wget curl git-core \
        vim-tiny zip unzip \
        python3 python3-pip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        gcc g++ default-jdk iproute2 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

RUN apt-get update && \
    apt-get install -y software-properties-common && \
        add-apt-repository ppa:deadsnakes/ppa && \
	apt-get update && \
	apt-get install -y python3.8 python3.8-dev && \
	apt-get install -y cmake && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PYTHONUNBUFFERED=1 \
    PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/spark/bin:/opt/spark/sbin \
    LANG=C.UTF-8 \
    PYSPARK_PYTHON=/usr/bin/python3 \
    SPARK_HOME=/opt/spark 

RUN curl https://bootstrap.pypa.io/get-pip.py | python3.8 && \
    python3.8 -m pip install --no-cache-dir -U setuptools && \
    python3.8 -m pip install --no-cache-dir h5py && \
    python3.8 -m pip install --no-cache-dir Cython && \
    python3.8 -m pip install --no-cache-dir matplotlib bokeh && \
    python3.8 -m pip install --no-cache-dir versioneer==0.17 && \
    python3.8 -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python3.8 -m pip install --no-cache-dir pandas && \
    python3.8 -m pip install --no-cache-dir seaborn && \
    python3.8 -m pip install --no-cache-dir pillow && \
    python3.8 -m pip install --no-cache-dir networkx cvxpy && \
    python3.8 -m pip install --no-cache-dir scikit-learn scikit-image && \
    python3.8 -m pip install --no-cache-dir pygments && \
    python3.8 -m pip install --no-cache-dir ipython && \
    python3.8 -m pip install --no-cache-dir jupyter && \
    python3.8 -m pip install --no-cache-dir jupyterlab && \
    python3.8 -m pip install --no-cache-dir pyspark && \ 
    rm -rf /root/.cache && \
    rm -f /tmp/*.whl
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.8 2
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.8 2

# Install ipython kernelspec
RUN python3.8 -m ipykernel install --display-name "Python 3.8 on Backend.AI" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

RUN wget -P /home/work https://downloads.apache.org/spark/spark-3.0.1/spark-3.0.1-bin-hadoop2.7.tgz
WORKDIR /home/work
RUN tar xvzf spark-3.0.1-bin-hadoop2.7.tgz && rm -rf spark-3.0.1-bin-hadoop2.7.tgz
RUN mv spark-3.0.1-bin-hadoop2.7 /opt/spark
COPY bootstrap.sh /opt/container/bootstrap.sh
RUN chmod 755 /opt/container/bootstrap.sh

# Backend.AI specifics
COPY ./service-defs /etc/backend.ai/service-defs
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8070,jupyterlab:http:8090,spark:preopen:8080"
COPY policy.yml /etc/backend.ai/jail/policy.yml

#EXPOSE 7077 8080
# vim: ft=dockerfile
