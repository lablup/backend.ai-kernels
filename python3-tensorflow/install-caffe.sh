#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.4

cd /home/sorna/caffe
make all
