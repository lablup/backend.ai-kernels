# backend.ai-kernels

Backend.AI agent kernels in various programming languages / toolkits and frameworks.

## Supporting modes

Here we list the latest versions of our supported kernel images.
"\*" in the Query mode column means that it supports preservation of global contexts across different query runs.

| Language      | Image Name              | Version         | Batch | Query | Input Hook | TTY | Runtime Impl. |
|---------------|-------------------------|-----------------|-------|-------|---|---|--------------------|
| C             | `lablup/kernel-c`       | 6.3             | O     | O     | O |   | GCC on Alpine 3.7  |
| C++ (14)      | `lablup/kernel-cpp`     | 6.3             | O     | O     |   |   | GCC on Alpine 3.7  |
| Go            | `lablup/kernel-go`      | 1.9             | O     | O     |   |   |                    |
| Haskell       | `lablup/kernel-haskell` | 8.2             | O     | O     |   |   |                    |
| Java          | `lablup/kernel-java`    | 8.0             | O     | O     |   |   |                    |
| Linux Console | `lablup/kernel-git`     | -               | -     | -     | - | O | Bash on Alpine 3.7 |
| Lua           | `lablup/kernel-lua`     | 5.3             | O     | O     |   |   |                    |
| Node.js       | `lablup/kernel-nodejs`  | 6.14,8.11,9.11  | O     | O     |   |   |                    |
| Octave        | `lablup/kernel-octave`  | 4.2             | O     | O     |   |   |                    |
| ~Python~      | `lablup/kernel-python`  | 2.7             | O     | O     | O |   | temporarily unsupported |
| Python        | `lablup/kernel-python`  | 3.6             | O     | O\*   | O |   |                    |
| Rust          | `lablup/kernel-rust`    | 1.17            | O     | O     |   |   |                    |
| PHP           | `lablup/kernel-php`     | 7.1             | O     | O     |   |   |                    |
| R             | `lablup/kernel-r`       | 3.3             | O     | O     |   |   | CRAN R             |
| Scala		| `lablup/kernel-scala`   | 2.12            | O     | O     |   |   |                    |

| Deep-Learning Framework | Image Name           | Version | Batch | Query | Input Hook | TTY | Runtime Impl. |
|------------|-----------------------------------|---------|-------|-------|-----|---|-------------------|
| TensorFlow | `lablup/kernel-python-tensorflow` | 1.9     | O     | O\*   | O   |   | Bundled w/Keras 2 |
| TensorFlow | `lablup/kernel-python-tensorflow` | 1.3     | O     | O\*   | O   |   | Bundled w/Keras 2 |
| PyTorch    | `lablup/kernel-python-torch`      | 0.2     | O     | O\*   | O   |   |                   |
| Theano     | `lablup/kernel-python-theano`     | 1.0     | O     | O\*   | O   |   | Bundled w/Keras 2 |
| CNTK       | `lablup/kernel-python-cntk`       | (WIP)   | O     | O\*   | O   |   | Bundled w/Keras 2 |

## Build chain

Kernels have dependencies to reduce total amount of storage, especailly for faster agent / instance setup and cost reduction. Default docker images are:

 * `base-alpine` (smallest base image)
 * `base-debian` (easier to compile common frameworks with libraries)
 * `base-ubuntu` (heaviest but best compatibility)
 * `base-mkl`    (Python conda distributed kernel based on Intel's Machine Learning Kits)

| base-alpine             | base-debian                  | base-ubuntu             |
|-------------------------|------------------------------|-------------------------|
| base-python-minimal-2.7 |                              |                         |
| base-python-minimal-3.6 | base-python-minimal-3.6      | base-python-minimal-3.6 |
| base-python-wheels-2.7  |                              |                         |
| base-python-wheels-3.6  |                              |                         |
| c-gcc6.3-alpine         |                              |                         |
| cpp-gcc6.3-alpine       |                              |                         |
| java-8-alpine           |                              |                         |
| rust-1.17-alpine        |                              |                         |
| git-3.6-alpine          |                              |                         |
| go-1.8-alpine           |                              |                         |
| go-1.9-alpine           |                              |                         |
|                         | haskell-ghc8.2-debian        |                         |
|                         | octave-4.2-debian            |                         |
| r-3.3-alpine            |                              |                         |
| lua-5.1-alpine          |                              |                         |
| lua-5.2-alpine          |                              |                         |
| lua-5.3-alpine          |                              |                         |
| php-7-alpine            |                              |                         |
| nodejs-6-alpine         |                              |                         |
| nodejs-8-alpine         |                              |                         |
| nodejs-9-alpine         |                              |                         |
|                         | julia-0.6-debian             |                         |
| r-3.3-alpine            |                              |                         |
| scala-2.12-alpine       |                              |                         |
|                         | bazel-0.7-debian             |                         |
|                         | bazel-0.11-debian            |                         |
|                         | cuda-9.2-cudnn-7.1-dev       |                         |
|                         | cuda-9.1-cudnn-7.1           |                         |
|                         | cuda-9.0-cudnn-7.1-dev       |                         |
|                         | cuda-9.0-cudnn-7.1           |                         |
|                         | cuda-8.0-cudnn-6.0           |                         |
|                         | python-caffe-1.0-py36        |                         |
|                         | python-torch-0.2-py36        |                         |
|                         | python-torch-0.2-py36-gpu    |                         |
|                         |                              | python-cntk-2.2-py36    |
|                         | tensorflow-1.9-py36-dense    |                         |
|                         | tensorflow-1.9-py36-dense-gpu|                         |
|                         | tensorflow-1.8-py36-dense    |                         |
|                         | tensorflow-1.8-py36-dense-gpu|                         |
|                         | tensorflow-1.7-py36-dense    |                         |
|                         | tensorflow-1.7-py36-dense-gpu|                         |
|                         | tensorflow-1.6-py36-dense    |                         |
|                         | tensorflow-1.6-py36-dense-gpu|                         |
|                         | tensorflow-1.5-py36-dense    |                         |
|                         | tensorflow-1.5-py36-dense-gpu|                         |
|                         | tensorflow-1.4-py36-dense    |                         |
|                         | tensorflow-1.4-py36-dense-gpu|                         |
|                         | tensorflow-1.3-py36-dense    |                         |
|                         | tensorflow-1.3-py36-dense-gpu|                         |

For the complete build chain, please refer `build.py` for more information.
