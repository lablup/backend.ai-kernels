#/bin/bash

tensorboard --logdir=/home/work/logs --port=8888 &
/home/backend.ai/pserver.py &
/usr/local/bin/python -m ai.backend.kernel python
