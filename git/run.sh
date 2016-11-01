#! /bin/bash
eval "$(pyenv init -)"
pyenv shell $SORNA_PYTHON_VERSION
git config --global alias.pretty-log 'log --graph --branches --tags --oneline --decorate'
exec /home/sorna/jail git `pyenv which python` /home/sorna/run.py
