#! /bin/bash
eval "$(pyenv init -)"
pyenv shell 3.4.4

git clone https://github.com/BVLC/caffe /home/sorna/caffe
cd /home/sorna/caffe
cp Makefile.config.example Makefile.config
sed -i 's@# CPU_ONLY@CPU_ONLY@ ' Makefile.config
sed -i 's@/usr/include/python2.7@/home/sorna/.pyenv/versions/python3.4.4/include/python3.4m@' Makefile.config
sed -i 's@/usr/lib/python2.7/dist-packages/numpy@/home/sorna/.pyenv/versions/python3.4.4/lib/python3.4/site-packages/numpy@' Makefile.config
sed -i 's@# PYTHON_LIBRARIES := boost_python3@PYTHON_LIBRARIES := boost_python3@' Makefile.config
pip3 install -r python/requirements.txt
