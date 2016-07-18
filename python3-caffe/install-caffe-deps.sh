#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.5.2

git clone https://github.com/BVLC/caffe /home/sorna/caffe-install
cd /home/sorna/caffe-install
cp Makefile.config.example Makefile.config
sed -i 's@# \(CPU_ONLY\)@\1@ ' Makefile.config
sed -i 's@\(INCLUDE_DIRS := .*\)@\1 /usr/local/include /usr/include/hdf5/serial/@' Makefile.config
sed -i 's@\(LIBRARY_DIRS := .*\)@\1 /usr/local/lib /usr/lib /usr/lib/x86_64-linux-gnu/hdf5/serial/@' Makefile.config
sed -i 's@/usr/include/python2.7@/home/sorna/.pyenv/versions/3.5.2/include/python3.5m@' Makefile.config
sed -i 's@/usr/lib/python2.7/dist-packages/numpy@/home/sorna/.pyenv/versions/3.5.2/lib/python3.5/site-packages/numpy@' Makefile.config
sed -i 's@\(PYTHON_LIB := .*\)@\1 /usr/lib/x86_64-linux-gnu /home/sorna/.pyenv/versions/3.5.2/lib@' Makefile.config
sed -i 's@# \(PYTHON_LIBRARIES := boost_python3\)@\1@' Makefile.config
pip install -r python/requirements.txt
