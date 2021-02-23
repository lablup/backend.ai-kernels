#!/bin/bash

# 오류가 발생하면 스크립트 종료
set -e

## 사용자가 마운트한 가상폴더 이름 가져오기
directories=$(ls -d */)
work_dir=$(echo $directories | cut -d"/" -f 1)
echo $work_dir

## Dockerfile
mkdir /home/work/$work_dir/venv
cp -r /home/venv/* /home/work/$work_dir/venv/
env PATH="/home/work/venv/bin:$PATH"

mkdir -p /home/work/$work_dir/model-server/
cp -r /home/model-server/* /home/work/$work_dir/model-server/
env TEMP=/home/$work_dir/model-server/tmp

cd /home/work/$work_dir


if [[ "$1" = "serve" ]]; then
    export LOG_LOCATION=/home/work/$work_dir
    echo "test export variation"
    shift 1
    torchserve --start --ts-config /home/work/$work_dir/model-server/config.properties \
                --model-store /home/work/$work_dir/model-server/model-store
else
    eval "$@"
fi

# prevent docker exit
tail -f /dev/null


