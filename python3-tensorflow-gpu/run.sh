#! /bin/bash
eval "$(pyenv init -)"
pyenv shell $SORNA_PYTHON_VERSION
exec /home/sorna/jail -policy /home/sorna/tensorflow.yml `pyenv which python` /home/sorna/run.py
