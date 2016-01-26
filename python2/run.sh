#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.11
exec /home/sorna/jail python2 `pyenv which python` /home/sorna/run.py
