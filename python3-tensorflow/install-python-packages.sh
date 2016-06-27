#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.1
pip3 install --upgrade python-dateutil
pip3 install pyzmq namedlist
pip3 install numpy
pip3 install matplotlib
pip3 install scipy
pip3 install cvxpy
pip3 install seaborn
pip3 install scikit-learn scikit-image
pip3 install pandas
pyenv rehash
