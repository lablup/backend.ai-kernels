FROM lablup/common-base:20.12-py36-cuda10.1

ARG TF_BUILD_VERSION=r2.4
# Install the most recent bazel release.
ENV BAZEL_VERSION 3.1.0

# Set up Bazel.

# Running bazel inside a `docker build` command causes trouble, cf:
#   https://github.com/bazelbuild/bazel/issues/134
# The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >>/etc/bazel.bazelrc
# Similarly, we need to workaround sandboxing issues:
#   https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" \
    >>/etc/bazel.bazelrc

WORKDIR /
RUN mkdir /bazel && \
    cd /bazel && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE && \
    chmod +x bazel-*.sh && \
    ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh && \
    cd / && \
    rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh


RUN python3 -m pip install --no-cache-dir -U \
    	    mesh-tensorflow==0.1.16 \
	    cachetools==3.1.0 \
	    tensorflow-transform==0.26.0 \
	    tensorflow_model_analysis==0.22.2 \
	    ngraph-tensorflow-bridge==0.18.0 \
	    tensorflow-metadata==0.26.0 \
	    tensorflow-datasets==3.2.0 \
	    tensorflow_probability==0.10.1 \
    	    tensorwatch==0.9.1 \
	    tensorflow-hub==0.8.0 \
	    tensorflow-addons==0.10.0 \
	    tensorflow_text==2.3.0 \
	    neural-structured-learning==1.1.0 \
	    tensorflow_constrained_optimization \
	    tensorflow-graphics-gpu==1.0.0 \
	    tensorflow-gan==2.0.0 \
	    tensorflow-data-validation==0.26.0 \
	    tensorflow-model-optimization==0.3.0 
RUN python3 -m pip install --no-cache-dir --upgrade \
	    jupyter-tensorboard==0.2.0 \
	    tf-agents==0.5.0 \
	    tf-slim==1.1.0 \
	    tensorflow-plot==0.3.2 \
	    tensorflow-lattice==2.0.5 \
	    tensorflow-io==0.15.0  \
	    tfx==0.22.1 \
	    tfx-bsl==0.22.1 
RUN python3 -m pip install --no-cache-dir \
	    tensorflow_ranking==0.3.1 \
	    tensorflow-compression==1.3 \
	    tensor2tensor==1.15.7 \
	    jupyterlab-nvdashboard==0.3.1 

# Download and build TensorFlow.
WORKDIR /tensorflow

# Download and build TensorFlow.
# Enable checking out both tags and branches
RUN export TAG_PREFIX="v" && \
    echo ${TF_BUILD_VERSION} | grep -q ^${TAG_PREFIX}; \
    if [ $? -eq 0 ]; then \
        git clone --depth=1 https://github.com/tensorflow/tensorflow.git . && \
        git fetch --tags && \
        git checkout ${TF_BUILD_VERSION}; \
   else \
        git clone --depth=1 --branch=${TF_BUILD_VERSION} https://github.com/tensorflow/tensorflow.git . ; \
    fi

RUN yes "" | python3 configure.py
RUN cp .bazelrc /root/.bazelrc

ENV CI_BUILD_PYTHON ${PYTHON}
ENV WHL_DIR=/tmp/pip3
# Set bazel build parameters in .bazelrc in parameterized_docker_build.sh
# Use --copt=-march values to get optimized builds appropriate for the hardware
#   platform of your choice.
# For ivy-bridge or sandy-bridge
# --copt=-march="avx" \
# For haswell, broadwell, or skylake
# --copt=-march="avx2" \
COPY .bazelrc /root/.mkl.bazelrc
RUN echo "import /root/.mkl.bazelrc" >>/root/.bazelrc

#ENV TF_NEED_TENSORRT=1
ENV TF_CUDA_COMPUTE_CAPABILITIES sm_35,sm_37,sm_52,sm_60,sm_61,sm_70,sm_75,compute_70,compute_75

RUN tensorflow/tools/ci_build/builds/configured GPU  \
      bazel --bazelrc=/root/.bazelrc build \
        -c opt \
        --copt=-msse4.1 \
        --copt=-msse4.2 \
	--copt=-mavx \
	--copt=-mavx2 \
	--copt=-mfma \
	--copt=-mfpmath=both \ 
	--copt=-O3 \
	--copt=-Wformat \
	--copt=-Wformat-security \
	--copt=-fstack-protector \
	--copt=-fPIC \
	--copt=-fpic \
	--config=opt \
	--config=cuda \
        --config=mkl \
	--config=monolithic \
        --config=gdr \ 
        --config=verbs \
#        --config=ngraph \
        --config=numa \
        --config=v2 \
	--linkopt=-znoexecstack \
	--linkopt=-zrelro \
	--linkopt=-znow \
	--linkopt=-fstack-protector \
	--linkopt -ldl \
        --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" \
      -k //tensorflow/tools/pip_package:build_pip_package && \
    bazel-bin/tensorflow/tools/pip_package/build_pip_package "${WHL_DIR}" && \
    python3 -m pip --no-cache-dir install --upgrade "${WHL_DIR}"/tensorflow-*.whl 
RUN python3 -m pip --no-cache-dir install \
        tensorboard==2.4 && \
    rm -rf /root/.cache

# Clean up Bazel cache when done.

# Install Horovod, temporarily using CUDA stubs
RUN ldconfig /usr/local/cuda/targets/x86_64-linux/lib/stubs && \
    HOROVOD_GPU_ALLREDUCE=NCCL HOROVOD_GPU_BROADCAST=NCCL HOROVOD_NCCL_LINK=SHARED \
    HOROVOD_WITH_TENSORFLOW=1 HOROVOD_WITHOUT_PYTORCH=1 HOROVOD_WITHOUT_MXNET=1\
    HOROVOD_GPU=CUDA \
    python3 -m pip install --no-cache-dir horovod==0.21.0 && \
    ldconfig

RUN python3 -m pip install --no-cache-dir \
            mpi4py==3.0.3 \
            nni==1.9 \
            mlflow==1.12.1 \
            scikit-nni==0.2.1


RUN python3 -m pip install --no-cache-dir --extra-index-url \
    	    https://developer.download.nvidia.com/compute/redist \
	    nvidia-dali-cuda110
#RUN python3 -m pip install --no-cache-dir \
#    	    --extra-index-url https://developer.download.nvidia.com/compute/redist nvidia-dali-tf-plugin-cuda110 
#WORKDIR /tmp
#RUN git clone  --recursive https://github.com/NVIDIA/DALI && \
#    cd DALI && \
#    mkdir build && \
#    cd build && \
#    cmake -D CMAKE_BUILD_TYPE=Release .. && \
#    make -j"$(nproc)" && \
#    cd .. && \
#    python3 -m pip install --no-cache-dir dali/python
    	

	    
# Install ipython kernelspec
Run python3 -m ipykernel install --display-name "TensorFlow 2.4 on Python 3.6 & CUDA 10.1" && \
    cat /usr/local/share/jupyter/kernels/python3/kernel.json

# Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.resource.min.cuda.device=0 \
      ai.backend.resource.min.cuda.shares=0 \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090,vscode:http:8180,tensorboard:http:6006"

WORKDIR /home/work
# vim: ft=dockerfile
