#! /bin/bash
eval "$(pyenv init -)"
pyenv install 3.4.3
pyenv shell 3.4.3
pyenv rehash
pip3 install --upgrade -q pip
pip3 install wheel
