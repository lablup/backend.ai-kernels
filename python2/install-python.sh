#! /bin/bash
eval "$(pyenv init -)"
pyenv install 2.7.11
pyenv shell 2.7.11
pyenv rehash
pip install --upgrade -q pip
pip install wheel
