#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
pip3 install --upgrade python-dateutil
pip3 install --upgrade protobuf==3.0.0b3

cd /home/sorna/caffe-install
make pycaffe
