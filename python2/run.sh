#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.10
exec /home/sorna/jail python2 `pyenv which python` /home/sorna/run.py
