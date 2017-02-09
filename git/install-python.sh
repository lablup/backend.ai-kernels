#! /bin/bash
eval "$(pyenv init -)"
pyenv install $SORNA_PYTHON_VERSION
pyenv shell $SORNA_PYTHON_VERSION
pyenv rehash
pip3 install -U -q pip wheel setuptools
