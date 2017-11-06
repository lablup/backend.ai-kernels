#! /bin/bash
eval "$(pyenv init -)"
pyenv shell $SORNA_PYTHON_VERSION
PYVER_M=$(echo $SORNA_PYTHON_VERSION | sed 's/\([0-9]\{1,\}\).\([0-9]\{1,\}\).\([0-9]\{1,\}\)/\1/')
PYVER_M_m=$(echo $SORNA_PYTHON_VERSION | sed 's/\([0-9]\{1,\}\).\([0-9]\{1,\}\).\([0-9]\{1,\}\)/\1.\2/')
PYVER_Mm=$(echo $SORNA_PYTHON_VERSION | sed 's/\([0-9]\{1,\}\).\([0-9]\{1,\}\).\([0-9]\{1,\}\)/\1\2/')

git clone https://github.com/BVLC/caffe /home/sorna/caffe-install
cd /home/sorna/caffe-install
cp Makefile.config.example Makefile.config
sed -i 's@# \(CPU_ONLY\)@\1@ ' Makefile.config
sed -i 's@\(INCLUDE_DIRS := .*\)@\1 /usr/local/include /usr/include/hdf5/serial/@' Makefile.config
sed -i 's@\(LIBRARY_DIRS := .*\)@\1 /usr/local/lib /usr/lib /usr/lib/x86_64-linux-gnu/hdf5/serial/@' Makefile.config
sed -i "s@/usr/include/python2.7@/home/sorna/.pyenv/versions/$SORNA_PYTHON_VERSION/include/python${PYVER_M_m}m@" Makefile.config
sed -i "s@/usr/lib/python2.7/dist-packages/numpy@/home/sorna/.pyenv/versions/$SORNA_PYTHON_VERSION/lib/python${PYVER_M_m}/site-packages/numpy@" Makefile.config
sed -i "s@\(PYTHON_LIB := .*\)@\1 /usr/lib/x86_64-linux-gnu /home/sorna/.pyenv/versions/$SORNA_PYTHON_VERSION/lib@" Makefile.config
sed -i 's@# \(PYTHON_LIBRARIES := boost_python3\)@\1@' Makefile.config
sed -i "s@boost_python3@boost_python${PYVER_M}@" Makefile.config
sed -i "s@python3.5@python${PYVER_M_m}@" Makefile.config
pip install -r python/requirements.txt

# Force upgrade of dateutil for Python 3 support
pip install -U 'python-dateutil>=2'
