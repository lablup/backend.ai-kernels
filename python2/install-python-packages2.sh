#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.10
pip install networkx
pyenv rehash
