#! /bin/bash
JULIA_COMPILED=/home/work/.julia/compiled/v1.0
if [ ! -d "$JULIA_COMPILED" ]; then
  mkdir -p $JULIA_COMPILED
fi
cp /opt/julia/compiled/v1.0/* $JULIA_COMPILED
jupyter notebook --no-browser --config $1