#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.3
pip3 install pyzmq namedlist
pip3 install numpy matplotlib scipy
pyenv rehash
