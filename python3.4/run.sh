#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.3
exec /home/sorna/jail python3 `pyenv which python` /home/sorna/run.py
