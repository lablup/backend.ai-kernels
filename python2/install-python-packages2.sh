#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.11
pip install networkx
pyenv rehash
