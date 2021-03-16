#!/bin/sh

USER_ID=${LOCAL_USER_ID:-9001}
USER_NAME=work

export HOME="/home/$USER_NAME"

su - $USER_NAME -c "touch /home/work/ffmpeglog"

if (! [ -z ${INPUT_STREAM}] ) && ( ! [ -z ${OUTPUT_STREAM} ] ); then
    echo "nohup /usr/local/bin/ffmpeg -i $INPUT_STREAM -vcodec hevc_nvenc -f rtsp $OUTPUT_STREAM >> /home/work/ffmpeglog &" >> /home/work/ffmpeglog
    su - $USER_NAME -c "nohup /usr/local/bin/ffmpeg -i $INPUT_STREAM -vcodec hevc_nvenc -f rtsp $OUTPUT_STREAM >> /home/work/ffmpeglog &"
else
    echo "INPUT_STREAM: $INPUT_STREAM" >> /home/work/ffmpeglog
    echo "OUTPUT_STREAM: $OUTPUT_STREAM" >> /home/work/ffmpeglog
fi
