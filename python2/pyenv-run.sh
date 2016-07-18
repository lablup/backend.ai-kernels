#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.12
exec "$@"
