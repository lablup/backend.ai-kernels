#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.4
exec `pyenv which python` /home/sorna/run.py
