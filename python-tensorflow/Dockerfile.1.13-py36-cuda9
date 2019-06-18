FROM lablup/common-tensorflow:1.13-py36-cuda9 as tf-binary
# import python-binary
FROM lablup/kernel-base:python3.6 as python-binary

FROM nvidia/cuda:9.0-cudnn7-runtime-ubuntu16.04
MAINTAINER Mario Cho "m.cho@lablup.com"

RUN apt-get update -y && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y "deb http://security.ubuntu.com/ubuntu xenial-security main" && \
    apt-get update -y && \
    apt-get install -y \
        ca-certificates \
        gcc g++ make \
	gfortran \
	vim \
	cmake \
	yasm \
	pkg-config \
        xz-utils \
        wget curl git-core \
        vim-tiny zip unzip \
        libssl-dev \
        libmpdec2 \
        proj-bin libproj-dev \
        libgeos-dev libgeos++-dev \
        mime-support \
        libpq-dev \
	libjasper-dev \
        libtiff-dev \
        libjpeg-dev \
        libpng-dev \
	libavcodec-dev \
        libavformat-dev \
	libswscale-dev \
	libxine2-dev \
	libv4l-dev

RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - && \
    apt-get update -y && \
    apt-get install -y nodejs 

RUN ln -s /usr/include/libv4l1-videodev.h /usr/include/linux/videodev.h && \
   apt-get install -y \
	libgstreamer1.0-dev \
	libgstreamer-plugins-base1.0-dev \
	libgtk-3-dev \
        libtbb-dev \
	libatlas-base-dev \
	libdc1394-22-dev \
	libxvidcore-dev \
	libfaac-dev \
	libmp3lame-dev \
	libtheora-dev \
	libvorbis-dev \
	libxvidcore-dev \
	libopencore-amrnb-dev libopencore-amrwb-dev \
	libavresample-dev \
	x264 \
	libx264-dev \
	v4l-utils \	
	libprotobuf-dev protobuf-compiler \
	libgoogle-glog-dev libgflags-dev \
	libgphoto2-dev \
	libeigen3-dev \
	libhdf5-dev \	
    && \	
    apt-get clean && \
    rm -rf /var/lib/apt/lists/

ENV PYTHONUNBUFFERED=1 \
    LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" \
    LANG=C.UTF-8

# Install CUDA-9.0 + cuDNN 7.6
RUN ln -s /usr/local/cuda-9.0 /usr/local/cuda && \
    ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.6.0 /usr/local/cuda/lib64/libcudnn.so && \
    ldconfig

COPY --from=python-binary /python.tar.gz /
RUN cd /; tar xzpf python.tar.gz; rm python.tar.gz; ldconfig

RUN curl https://bootstrap.pypa.io/get-pip.py | python3 && \
    python3 -m pip install --no-cache-dir -U setuptools && \
    python3 -m pip install --no-cache-dir wheel && \
    python3 -m pip install --no-cache-dir h5py && \
    python3 -m pip install --no-cache-dir Cython && \
    python3 -m pip install --no-cache-dir requests && \
    python3 -m pip install --no-cache-dir numpy scipy && \
    python3 -m pip install --no-cache-dir pyzmq simplejson msgpack-python uvloop && \
    python3 -m pip install --no-cache-dir aiozmq dataclasses tabulate && \
    python3 -m pip install --no-cache-dir namedlist six "python-dateutil>=2" && \
    python3 -m pip install --no-cache-dir versioneer==0.17 && \
    python3 -m pip install --no-cache-dir pyproj Cartopy==0.16 && \
    python3 -m pip install --no-cache-dir matplotlib bokeh && \
    python3 -m pip install --no-cache-dir pandas && \
    python3 -m pip install --no-cache-dir seaborn && \
    python3 -m pip install --no-cache-dir networkx cvxpy && \
    python3 -m pip install --no-cache-dir scikit-learn scikit-image && \
    python3 -m pip install --no-cache-dir pygments && \
    python3 -m pip install --no-cache-dir future && \
    python3 -m pip install --no-cache-dir tensorwatch

WORKDIR /tmp
ENV OPENCV_VERSION="4.1.0"

RUN wget https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip && \
    wget -O opencv-contrib.zip https://github.com/opencv/opencv_contrib/archive/${OPENCV_VERSION}.zip && \
    unzip ${OPENCV_VERSION}.zip && \
    unzip opencv-contrib.zip && \
    mkdir opencv-${OPENCV_VERSION}/cmake_binary && \
    cd opencv-${OPENCV_VERSION}/cmake_binary && \
    cmake \
      -DCMAKE_BUILD_TYPE=RELEASE \
      -D BUILD_TIFF=ON \
      -D BUILD_opencv_java=OFF \
      -D WITH_CUDA=ON \
      -D CUDA_NVCC_FLAGS=--expt-relaxed-constexpr \
      -D CUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda-9.2 \
      -D ENABLE_FAST_MATH=1 \
      -D CUDA_FAST_MATH=1 \
      -D WITH_CUBLAS=1 \
      -D WITH_OPENGL=ON \
      -D WITH_OPENCL=ON \
      -D WITH_IPP=ON \
      -D WITH_TBB=ON \
      -D WITH_EIGEN=ON \
      -D WITH_V4L=ON \
      -D BUILD_TESTS=OFF \
      -D BUILD_PERF_TESTS=OFF \
      -D OPENCV_EXTRA_MODULES_PATH="../../opencv_contrib-4.1.0/modules" \
      -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=$(python3 -c "import sys; print(sys.prefix)") \
      -D PYTHON_EXECUTABLE=$(which python3) \
      -D PYTHON_INCLUDE_DIR=$(python3 -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())") \
      -D PYTHON_PACKAGES_PATH=$(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())") \
      .. 2>&1 | tee cmake_messages.txt && \
    make -j${nproc} && \
    make install && \
    cd /tmp && \
    rm -fr opencv* 

COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp


# Jupyter notebook extension 
RUN mkdir -p /home/work/.jupyter/nbextension
WORKDIR /home/work/.jupyter/nbextension

RUN python3 -m pip install --no-cache-dir opencv-python && \
    python3 -m pip install --no-cache-dir wheel /tmp/*.whl && \
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
RUN python3 -m ipykernel install --display-name "TensorFlow 1.13 on Python 3.6 (CUDA 9.0)" && \
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
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.shares=0.1 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/local/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"

COPY policy.yml /etc/backend.ai/jail/policy.yml

# vim: ft=dockerfile