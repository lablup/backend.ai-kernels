#! /bin/bash
eval "$(pyenv init -)"
pyenv shell $SORNA_PYTHON_VERSION
exec /home/sorna/jail python `pyenv which python` /home/sorna/run.py
