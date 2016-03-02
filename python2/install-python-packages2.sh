#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 2.7.11
pip install networkx
pyenv rehash

cat > /tmp/matplotlib-fontcache.py <<- EOF
import matplotlib.pyplot
EOF
python3 /tmp/matplotlib-fontcache.py
rm /tmp/matplotlib-fontcache.py
