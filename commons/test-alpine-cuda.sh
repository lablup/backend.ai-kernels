#! /bin/sh
docker run --rm -it \
 -v nvidia_driver_375.26:/usr/local/nvidia:ro \
 -v nvidia_extra:/usr/local/nvidia-extra:ro \
 --device /dev/nvidiactl:/dev/nvidiactl:mrw \
 --device /dev/nvidia-uvm:/dev/nvidia-uvm:mrw \
 --device /dev/nvidia-uvm-tools:/dev/nvidia-uvm-tools:mrw \
 --device /dev/nvidia1:/dev/nvidia1:mrw \
 -e LD_LIBRARY_PATH=/usr/local/cuda/lib64:/usr/local/nvidia/lib64 \
 -e PATH=/usr/local/cuda/bin:/usr/local/nvidia/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
 lablup/kernel-python3-tensorflow-gpu /bin/bash
 #lablup/common-cuda:debian /bin/bash

#nvidia-docker run -it --name alpine-cuda-2 lablup/common-cuda:8.0 /bin/ash


# vim: tw=0
