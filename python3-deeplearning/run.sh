#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.4
exec /home/sorna/jail python `pyenv which python` /home/sorna/run.py
