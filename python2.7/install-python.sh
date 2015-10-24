#! /bin/bash
eval "$(pyenv init -)"
pyenv install 2.7.10
pyenv shell 2.7.10
pyenv rehash
pip install --upgrade -q pip
pip install wheel
