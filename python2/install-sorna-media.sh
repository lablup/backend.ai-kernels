#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.11
pip install /tmp/sorna_media-0.2.0-py2.py3-none-any.whl
