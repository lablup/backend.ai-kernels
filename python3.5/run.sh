#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
exec /home/sorna/jail python3 `pyenv which python` /home/sorna/run.py
