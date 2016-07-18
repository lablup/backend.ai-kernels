#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.2
exec "$@"
