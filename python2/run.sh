#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.12
exec /home/sorna/jail python `pyenv which python` /home/sorna/run.py
