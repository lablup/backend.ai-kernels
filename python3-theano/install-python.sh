#! /bin/bash
eval "$(pyenv init -)"
export PYTHON_CONFIGURE_OPTS="--enable-shared"
pyenv install $SORNA_PYTHON_VERSION
pyenv shell $SORNA_PYTHON_VERSION
pyenv rehash
pip3 install --upgrade -q pip
pip3 install wheel
