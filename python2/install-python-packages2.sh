#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.10
pip install networkx
pip install --upgrade wheel
pip install six https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.6.0-cp27-none-linux_x86_64.whl
pyenv rehash
