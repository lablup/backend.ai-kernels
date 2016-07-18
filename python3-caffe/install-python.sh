#! /bin/bash
eval "$(pyenv init -)"
export PYTHON_CONFIGURE_OPTS="--enable-shared"
pyenv install 3.5.2
pyenv shell 3.5.2
pyenv rehash
pip3 install --upgrade -q pip
pip3 install wheel
