#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
pip3 install networkx
pip3 install six
pip3 install python-dateutil
pyenv rehash

cat > /tmp/matplotlib-fontcache.py <<- EOF
import matplotlib.pyplot
EOF
python3 /tmp/matplotlib-fontcache.py
rm /tmp/matplotlib-fontcache.py
