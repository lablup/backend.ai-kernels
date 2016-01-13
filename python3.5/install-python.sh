#! /bin/bash
eval "$(pyenv init -)"
pyenv install 3.5.1
pyenv shell 3.5.1
pyenv rehash
pip3 install --upgrade -q pip
pip3 install wheel
