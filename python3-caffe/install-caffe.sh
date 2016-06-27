#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1

cd /home/sorna/caffe-install
make all -j4
