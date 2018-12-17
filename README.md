# backend.ai-kernels

 Backend.AI agent kernels in various programming languages / toolkits and frameworks.

## Supporting modes

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
| ~Python~      | `lablup/kernel-python`  | 2.7             | O     | O     | O |   | temporarily unsupported |
| Python        | `lablup/kernel-python`  | 3.6.6           | O     | O\*   | O |   |                    |
| Rust          | `lablup/kernel-rust`    | 1.17            | O     | O     |   |   |                    |
| PHP           | `lablup/kernel-php`     | 7.1             | O     | O     |   |   |                    |
| R             | `lablup/kernel-r`       | 3.3             | O     | O     |   |   | CRAN R             |
| Scala	        | `lablup/kernel-scala`   | 2.12            | O     | O     |   |   |                    |

| Deep-Learning Framework | Image Name           | Version | Batch | Query | Input Hook | TTY | Runtime Impl. |
|------------|-----------------------------------|---------|-------|-------|-----|---|-------------------|
| TensorFlow | `lablup/kernel-python-tensorflow` | 1.12    | O     | O\*   | O   |   | Bundled w/Keras 2 |
| PyTorch    | `lablup/kernel-python-torch`      | 1.0rc   | O     | O\*   | O   |   |                   |
| caffe2     | `lablup/kernel-python-torch`      | 1.0rc   | O     | O\*   | O   |   |                   |
| cafee      | `lablup/kernel-python-caffe`      | 1.0     | O     | O\*   | O   |   |                   |
| CNTK       | `lablup/kernel-python-cntk`       | (WIP)   | O     | O\*   | O   |   | Bundled w/Keras 2 |
| Chainer    | `lablup/kernel-python-chainer`    | (WIP)   | O     | O\*   | O   |   |                   |
| Theano     | `lablup/kernel-python-theano`     | 1.0     | O     | O\*   | O   |   | Bundled w/Keras 2 |

## Build chain

Kernels have dependencies to reduce total amount of storage, especailly for faster agent / instance setup and cost reduction. Default docker images are:

 * `base-alpine` (smallest base image)
 * `base-debian` (easier to compile common frameworks with libraries)
 * `base-ubuntu` (heaviest but best compatibility)

| base-alpine             | base-debian                  | base-ubuntu             | 
|-------------------------|------------------------------|-------------------------|
| base-python-minimal-2.7 |                              | base-python-minimal-2.7 |
| base-python-minimal-3.6 | base-python-minimal-3.6      | base-python-minimal-3.6 |
| base-python-wheels-2.7  |                              |                         |
| base-python-wheels-3.6  |                              |                         |
|                         |                              | base-ubuntu16.04-mkl2019|
| c-gcc6.3-alpine         |                              |                         |
| cpp-gcc6.3-alpine       |                              |                         |
| java-8-alpine           |                              |                         |
| rust-1.17-alpine        |                              |                         |
| git-3.6-alpine          |                              |                         |
| go-1.8-alpine           |                              |                         |
| go-1.9-alpine           |                              |                         |
| haskell-ghc8.2-alpine   |                              |                         |
| octave-4.2-alipine      |                              |                         |
| r-3.3-alpine            |                              |                         |
| lua-5.1-alpine          |                              |                         |
| lua-5.2-alpine          |                              |                         |
| lua-5.3-alpine          |                              |                         |
| php-7-alpine            |                              |                         |
| nodejs-6-alpine         |                              |                         |
| nodejs-8-alpine         |                              |                         |
| nodejs-10-alpine        |                              |                         |
|                         |                              | julia-0.6-ubuntu        |
| r-3.3-alpine            |                              |                         |
| scala-2.12-alpine       |                              |                         |

## Deep learning based images
 * `base-mkl`    (Intel' Machine Learning Kits (MKL) works on CPU only kernel) 
 * `base-cuda`   (Nvidia' GPU & CUDA libarary compatibility, needed Nvidia-docker)
 * `base-TPU`    (Google TPU comptibility, on Google' Cloud)
 * `base-ROCm`   (AMD' GPU & OpenCL libarary compatibility, T.B.D.)

| base-mkl                 | base-cuda                     | base-TPU                 | base-ROCm (T.B.D) |
| tensorflow-1.12-py36     | tensorflow-1.12-py36-cuda9    | tensorflow-1.12-py36-tpu |                   |
| tensorflow-1.11-py36     | tensorflow-1.11-py36-cuda9    | tensorflow-1.12-py36-tpu |                   |
| tensorflow-1.10-py36     | tensorflow-1.10-py36-cuda9    |                          |                   |   
| tensorflow-1.9-py36      | tensorflow-1.9-py36-cuda9     |                          |                   |   
| tensorflow-1.8-py36      | tensorflow-1.8-py36-cuda9     |                          |                   |   
| tensorflow-1.7-py36      | tensorflow-1.7-py36-cuda9     |                          |                   |   
| tensorflow-1.6-py36      | tensorflow-1.6-py36-cuda9     |                          |                   |   
| tensorflow-1.5-py36      | tensorflow-1.5-py36-cuda9     |                          |                   |   
|                          | tensorflow-1.4-py36-cuda8     |                          |                   |   
|                          | tensorflow-1.3-py36-cuda8     |                          |                   |
|                          | tensorflow-1.2-py36-cuda8     |                          |                   |
|                          | tensorflow-1.1-py36-cuda8     |                          |                   |
|                          | tensorflow-1.0-py36-cuda8     |                          |                   |
|                          | python-caffe-1.0-py36-cuda9   |                          |                   |   
|                          | python-torch-1.0-py36-cuda9   |                          |                   |   
|                          | python-torch-0.4-py36-cuda9   |                          |                   |   
|                          | python-torch-0.3-py36-cuda9   |                          |                   |   
|                          | python-torch-0.2-py36-cuda8   |                          |                   |   
|                          | python-cntk-2.2-py36          |                          |                   |   
|                          | python-cntk-2.2-py36          |                          |                   |   

## Deep learning inference images

 * `base`        (CPU only ) 
 * `base-cuda`   (Nvidia' GPU & CUDA libarary compatibility, needed Nvidia-docker)
 * `base-TPU`    (Google TPU comptibility, on Google' Cloud)
 * `base-ROCm`   (AMD' GPU & OpenCL libarary compatibility, T.B.D.)

| base                     |  base-cuda                     | base-TPU (T.B.D)         | base-ROCm (T.B.D) |
| tensorflow-1.12-py36-srv | tensorflow-1.12-py36-srv-cuda9 |                          |                   |   
| tensorflow-1.11-py36-srv | tensorflow-1.11-py36-srv-cuda9 |                          |                   |

For the complete build chain, please refer `build.py` for more information.
 