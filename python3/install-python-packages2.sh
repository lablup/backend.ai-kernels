#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
pip3 install networkx
pyenv rehash
