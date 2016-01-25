#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
pip3 install networkx
pip3 install six
cd /home/sorna
wget https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.6.0-cp34-none-linux_x86_64.whl
# A renaming trick to use it on Python 3.5
mv tensorflow-0.6.0-cp34-none-linux_x86_64.whl tensorflow-0.6.0-cp35-none-linux_x86_64.whl
pip install tensorflow-0.6.0-cp35-none-linux_x86_64.whl
rm tensorflow-0.6.0-cp35-none-linux_x86_64.whl
pyenv rehash
