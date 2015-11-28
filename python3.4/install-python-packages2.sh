#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.3
pip3 install networkx
pyenv rehash
