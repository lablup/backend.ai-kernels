# backend.ai-kernels

Backend.AI agent kernels in various programming languages / toolkits and frameworks.

## Supporting modes

("\*" in the Query mode column means that it supports preservation of global contexts across different query runs.)

| Language      | Version | Batch | Query | Input Hook | TTY | Runtime Implementation |
|---------------|------|---|---|-----|---|-------------------|
| C             | 6.3  | O | O | O   |   | GCC compiler      |
| C++ (C++14)   | 6.3  | O | O | O   |   | GCC compiler      |
| Go            | 1.9  | O | O |     |   |                   | 
| Haskell       | 8.2  | O | O |     |   |                   |
| Java          | 8.0  | O | O |     |   |                   |
| Linux Console | -    |   |   | O   | O | Bash On Ubuntu    |  
| Lua           | 5.1  |   | O |     |   |                   |
| Lua           | 5.2  |   | O |     |   |                   |
| Lua           | 5.3  |   | O |     |   |                   |
| Node.js       | 6    |   | O |     |   |                   |
| Octave        | 4.2  |   | O |     |   |                   |
| ~Python~      | 2.7  | O | O | O   |   | temporarily unsupported |
| Python        | 3.6  | O | O | O\* |   |                   | 
| Rust          | 1.17 | O | O |     |   |                   | 
| PHP           | 7.0  |   | O |     |   |                   |
| R             | 3.0  |   | O |     |   | CRAN R            |

| Deep-Learning Framework | Version  | Batch | Query | Input Hook | TTY | Runtime Implementation |
|---------------|------|---|---|-----|---|-------------------|
| TensorFlow    | 1.4  | O | O | O\* |   | Bundled w/Keras 2 |
| TensorFlow    | 1.3  | O | O | O\* |   | Bundled w/Keras 2 |
| PyTorch       | 0.2  | O | O | O\* |   |                   |
| Theano        | 0.9  | O | O | O\* |   | Bundled w/Keras 2 |
| CNTK          |(WIP) | O | O | O\* |   | Bundled w/Keras 2 |

## Build chain

Kernels have dependencies to reduce total amount of storage, especailly for faster agent / instance setup and cost reduction. Default docker images are:

 * `base-alpine` (smallest base image)
 * `base-debian` (easier to compile common frameworks with libraries)
 * `base-ubuntu` (heaviest but best compatibility)

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
|                         | julia-0.6-debian             |                         |
| r-3.3-alpine            |                              |                         |
|                         | bazel-0.7-debian             |                         |
|                         | cuda-8.0-cudnn-6.0           |                         |
|                         | python-caffe-1.0-py36        |                         |
|                         | python-torch-0.2-py36        |                         |
|                         | python-torch-0.2-py36-gpu    |                         |
|                         |                              | python-cntk-2.2-py36    |
|                         | tensorflow-1.4-py36-dense    |                         |
|                         | tensorflow-1.4-py36-dense-gpu|                         |
|                         | tensorflow-1.3-py36-dense    |                         |
|                         | tensorflow-1.3-py36-dense-gpu|                         |

For the complete build chain, please refer `build.py` for more information.
