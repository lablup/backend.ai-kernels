#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.4
pip3 install networkx
pip3 install six
pip3 install https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.7.1-cp34-none-linux_x86_64.whl
pyenv rehash

cat > /tmp/sample_data_importer.py <<- EOF
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)
print("Imported MNIST data.")
EOF
python3 /tmp/sample_data_importer.py
rm /tmp/sample_data_importer.py

cat > /tmp/matplotlib-fontcache.py <<- EOF
import matplotlib.pyplot
EOF
python3 /tmp/matplotlib-fontcache.py
rm /tmp/matplotlib-fontcache.py
