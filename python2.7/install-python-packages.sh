#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.10
pip install pyzmq namedlist
pip install numpy matplotlib scipy
pyenv rehash
