#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
pip3 install networkx
pip3 install six
pyenv rehash

cat > /tmp/matplotlib-fontcache.py <<- EOF
import sys
import matplotlib
print('Matplotlib Cache Directory:', matplotlib.get_cachedir(), file=sys.stderr)
import matplotlib.pyplot
EOF
python3 /tmp/matplotlib-fontcache.py
rm /tmp/matplotlib-fontcache.py
