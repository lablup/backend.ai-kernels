#! /bin/bash
eval "$(pyenv init -)"
pyenv install 3.4.4
pyenv shell 3.4.4
pyenv rehash
pip3 install --upgrade -q pip
pip3 install wheel
