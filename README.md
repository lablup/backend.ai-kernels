# Backend.AI Kernels Repository

Backend.AI agent kernels in various programming languages / toolkits and frameworks.


## Officially Supported Images

### Supporting modes

Here we list the latest versions of our supported kernel images.
"\*" in the Query mode column means that it supports preservation of global contexts across different query runs.

| Language      | Image Name              | Version         | Batch | Query | Input Hook | TTY | Runtime Impl. |
|---------------|-------------------------|-----------------|-------|-------|---|---|--------------------|
| C             | `lablup/kernel-c`       | 6.3             | O     | O     | O |   | GCC on Alpine 3.8  |
| C++ (14)      | `lablup/kernel-cpp`     | 6.3             | O     | O     |   |   | GCC on Alpine 3.8  |
| Go            | `lablup/kernel-go`      | 1.9             | O     | O     |   |   |                    |
| Haskell       | `lablup/kernel-haskell` | 8.2             | O     | O     |   |   |                    |
| Java          | `lablup/kernel-java`    | 8.0             | O     | O     |   |   |                    |
| Linux Console | `lablup/kernel-git`     | -               | -     | -     | - | O | Bash on Alpine 3.8 |
| Lua           | `lablup/kernel-lua`     | 5.3             | O     | O     |   |   |                    |
| Node.js       | `lablup/kernel-nodejs`  | 6.14,8.11,10.11 | O     | O     |   |   |                    |
| Octave        | `lablup/kernel-octave`  | 4.2             | O     | O     |   |   |                    |
| Python        | `lablup/kernel-python`  | 2.7             | O     | O     | O |   | beta               |
| Python        | `lablup/kernel-python`  | 3.6.6           | O     | O\*   | O |   |                    |
| Rust          | `lablup/kernel-rust`    | 1.17            | O     | O     |   |   |                    |
| PHP           | `lablup/kernel-php`     | 7.1             | O     | O     |   |   |                    |
| R             | `lablup/kernel-r`       | 3.3             | O     | O     |   |   | CRAN R             |
| Scala	        | `lablup/kernel-scala`   | 2.12            | O     | O     |   |   |                    |

| Deep-Learning Framework | Image Name    | Version | Batch | Query | Input Hook | TTY | Runtime Impl. |
|------------|----------------------------|---------|-------|-------|-----|---|-------------------|
| TensorFlow | `lablup/python-tensorflow` | 1.14    | O     | O\*   | O   |   | Bundled w/Keras 2 |
| PyTorch    | `lablup/python-torch`      | 1.1     | O     | O\*   | O   |   |                   |
| caffe2     | `lablup/python-torch`      | 1.0     | O     | O\*   | O   |   |                   |
| cafee      | `lablup/python-caffe`      | 1.0     | O     | O\*   | O   |   |                   |
| CNTK       | `lablup/python-cntk`       | 2.6     | O     | O\*   | O   |   | Bundled w/Keras 2 |
| Chainer    | `lablup/python-chainer`    | 4.0     | O     | O\*   | O   |   |                   |
| Theano     | `lablup/python-theano`     | 1.0     | O     | O\*   | O   |   | Bundled w/Keras 2 |
=======
| Deep-Learning Framework | Image Name           | Version | Batch | Query | Input Hook | TTY | Runtime Impl. |
|------------|-----------------------------------|---------|-------|-------|-----|---|-------------------|
| TensorFlow | `lablup/kernel-python-tensorflow` | 1.14    | O     | O\*   | O   |   | Bundled w/Keras 2 |
| PyTorch    | `lablup/kernel-python-torch`      | 1.1     | O     | O\*   | O   |   |                   |
| caffe2     | `lablup/kernel-python-torch`      | 1.0     | O     | O\*   | O   |   |                   |
| cafee      | `lablup/kernel-python-caffe`      | 1.0     | O     | O\*   | O   |   |                   |
| CNTK       | `lablup/kernel-python-cntk`       | 2.7     | O     | O\*   | O   |   | Bundled w/Keras 2 |
| Chainer    | `lablup/kernel-python-chainer`    | (WIP)   | O     | O\*   | O   |   |                   |
| Theano     | `lablup/kernel-python-theano`     | 1.0     | O     | O\*   | O   |   | Bundled w/Keras 2 |


### Deep learning based images
 * `base-mkl`    (Intel' Machine Learning Kits (MKL) works on CPU only kernel)
 * `base-cuda`   (Nvidia' GPU & CUDA libarary compatibility, needed Nvidia-docker)
 * `base-TPU`    (Google TPU comptibility, on Google' Cloud)
 * `base-ROCm`   (AMD' GPU & OpenCL libarary compatibility, T.B.D.)

| base-mkl                 | base-cuda                     | base-TPU                 | base-ROCm (T.B.D) |
|--------------------------|-------------------------------|--------------------------|-------------------|
| tensorflow-2.0-py36      | tensorflow-2.0-py36-cuda9     |                          |                   |
| tensorflow-1.14-py36     | tensorflow-1.14-py36-cuda9    | tensorflow-1.14-py36-tpu |                   |
| tensorflow-1.13-py36     | tensorflow-1.13-py36-cuda9    | tensorflow-1.13-py36-tpu |                   |
| tensorflow-1.12-py36     | tensorflow-1.12-py36-cuda9    | tensorflow-1.12-py36-tpu |                   |
| tensorflow-1.11-py36     | tensorflow-1.11-py36-cuda9    | tensorflow-1.11-py36-tpu |                   |
| tensorflow-1.10-py36     | tensorflow-1.10-py36-cuda9    |                          |                   |
| tensorflow-1.9-py36      | tensorflow-1.9-py36-cuda9     |                          |                   |
| tensorflow-1.8-py36      | tensorflow-1.8-py36-cuda9     |                          |                   |
| tensorflow-1.7-py36      | tensorflow-1.7-py36-cuda9     |                          |                   |
| tensorflow-1.6-py36      | tensorflow-1.6-py36-cuda9     |                          |                   |
| tensorflow-1.5-py36      | tensorflow-1.5-py36-cuda9     |                          |                   |
| tensorflow-1.4-py36      | tensorflow-1.4-py36-cuda8     |                          |                   |
| tensorflow-1.3-py36      | tensorflow-1.3-py36-cuda8     |                          |                   | 
| tensorflow-1.2-py36      | tensorflow-1.2-py36-cuda8     |                          |                   |
| tensorflow-1.1-py36      | tensorflow-1.1-py36-cuda8     |                          |                   |
| tensorflow-1.0-py36      | tensorflow-1.0-py36-cuda8     |                          |                   |
|                          | python-caffe2-1.0-py36-cuda9  |                          |                   |
| python-torch-1.1-py36    | python-torch-1.1-py36-cuda9   |                          |                   |
| python-torch-1.0-py36    | python-torch-1.0-py36-cuda9   |                          |                   |
| python-torch-0.4-py36    | python-torch-0.4-py36-cuda9   |                          |                   |
| python-torch-0.3-py36    | python-torch-0.3-py36-cuda9   |                          |                   |
| python-torch-0.2-py36    | python-torch-0.2-py36-cuda8   |                          |                   |
| python-cntk-2.7-py36     | python-cntk-2.7-py36-cuda9    |                          |                   |
| python-cntk-2.6-py36     | python-cntk-2.6-py36-cuda9    |                          |                   |
| python-cntk-2.5-py36     | python-cntk-2.5-py36-cuda9    |                          |                   |
| python-cntk-2.4-py36     | python-cntk-2.4-py36-cuda9    |                          |                   |
| python-cntk-2.3-py36     | python-cntk-2.3-py36-cuda9    |                          |                   |
| python-cntk-2.2-py36     | python-cntk-2.2-py36-cuda9    |                          |                   |
| python-cntk-2.1-py36     | python-cntk-2.1-py36-cuda9    |                          |                   |
| python-cntk-2.0-py36     | python-cntk-2.0-py36-cuda9    |                          |                   |

### Deep learning inference images

| base                     |  base-cuda                     | base-TPU (T.B.D)         | base-ROCm (T.B.D) |
|--------------------------|--------------------------------|--------------------------|-------------------|
| tensorflow-1.14-py36-srv | tensorflow-1.14-py36-srv-cuda9 |                          |                   |
| tensorflow-1.13-py36-srv | tensorflow-1.13-py36-srv-cuda9 |                          |                   |
| tensorflow-1.12-py36-srv | tensorflow-1.12-py36-srv-cuda9 |                          |                   |
| tensorflow-1.11-py36-srv | tensorflow-1.11-py36-srv-cuda9 |                          |                   |


## HOWTO: Adding a New Image

Since Backend.AI v19.03, the kernel-runner component are completely separated from
the kernel images as they are mounted at runtime by the agent.

All you need to do for a new kernel is specifying a set of Backend.AI-specific labels
and preparation of the jail policy.

* `ai.backend.kernelspec`: For now, it's set to "1".
* `ai.backend.features`: A list of constant strings indicating which Backend.AI
  kernel features are available for the kernel.
  - **batch**: Can execute user programs passed as files.
  - **query**: Can execute user programs passed as code snippets while keeping the
    context across multiple executions.
  - **uid-match**: As of 19.03, this must be specified always.
  - **user-input**: The query/batch mode supports interactive user inputs.
* `ai.backend.resource.min.*`: The minimum amount of resource to launch this kernel.
  At least, you must define the CPU core (`cpu`) and the main memory (`mem`).
  In the memory size values, you may use binary scale-suffixes such as `m` for `MiB`, `g` for `GiB`, etc.
* `ai.backend.base-distro`: Either "ubuntu16.04" or "alpine3.8".  Note that Ubuntu
  18.04-based kernels also need to use "ubuntu16.04" here.
* `ai.backend.runtime-type`: The type of kernel runner to use. (One of the
  directories in [the `ai.backend.kernel` namespace](https://github.com/lablup/backend.ai-agent/tree/master/src/ai/backend/kernel).)
* `ai.backend.runtime-path`: The path to the language runtime executable.
* `ai.backend.service-ports`: A list of 3-tuple strings specifying services available
  via network tunneling. Each tuple consists of the service name, the service type
  (one of **pty**, **http**, or **tcp**) and the container-side port number.
  Backend.AI manages the host-side port mapping and network tunneling via the API
  gateway automagically.
* `ai.backend.envs.corecount`: The list of environment variables to be set as the
  number of available CPU cores to the container.  This is for legacy parallel
  computation libraries.

Note that the implementation of query/batch modes, runtime-type and service-ports are
the responsibility of the kernel runner in the agent codebase.
For most computation kernels based Python (e.g., Anaconda, NVIDIA GPU Cloud, etc.)
may simply reuse the implementation and labels from the standard "python" image.

Currently we support two major base Linux distros, Ubuntu and Alpine.

### Example: An Ubuntu-based kernel

```dockerfile
FROM ubuntu:16.04

# Add commands for image customization
RUN apt-get install ...

# Backend.AI specifics
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec=1 \
      ai.backend.resource.min.cpu=1 \
      ai.backend.resource.min.mem=256m \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/opt/conda/bin/python" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080"
```

### Example: Kernels supporting accelerators

CUDA-accelerated:
```
...
LABEL ... \
      ai.backend.resource.min.cuda.device=1 \
      ai.backend.resource.min.cuda.smp=2 \
      ai.backend.resource.min.cuda.mem=256m \
      ...
...
```

TPU-accelerated:
```
...
LABEL ... \
      ai.backend.resource.min.tpu.device=1 \
      ...
...
```

### Example: An Alpine-based kernel

Alpine Linux requires two additional lines as it does not support the full-featured
`ldconfig`.

```dockerfile
FROM alpine:3.8

# Add commands for image customization
RUN apk add ...

# Backend.AI specifics
ENV LD_LIBRARY_PATH=/opt/backend.ai/lib
RUN apk add --no-cache libffi libzmq
COPY policy.yml /etc/backend.ai/jail/policy.yml
LABEL ai.backend.kernelspec=1 \
      ai.backend.resource.min.cpu=1 \
      ai.backend.resource.min.mem=256m \
      ai.backend.features="batch query uid-match" \
      ai.backend.base-distro="alpine3.8" \
      ai.backend.runtime-type="lua" \
      ai.backend.runtime-path="/usr/bin/lua" \
      ai.backend.service-ports=""
```
