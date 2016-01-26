#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.4
pip3 install networkx
pip3 install six
pip3 install https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.6.0-cp34-none-linux_x86_64.whl
pyenv rehash
