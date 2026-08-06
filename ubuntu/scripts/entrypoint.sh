#!/bin/bash
# set -e: exit asap if a command exits with a non-zero status
set -e
trap ctrl_c INT
function ctrl_c() {
  exit 0
}

[ ! -z "${RESOLUTION+x}" ] && export VNC_RESOLUTION="$RESOLUTION"
[ ! -d "/home/work/.config" ] && mkdir /home/work/.config
[ ! -d  "/home/work/.config/xfce4" ] && tar xzvf /usr/local/xfcedeskconfig/deskconfig.tar.gz -C /home/work/.config/ 

# entrypoint.sh file for starting the xvfb with better screen resolution, configuring and running the vnc server.
rm /tmp/.X1-lock 2> /dev/null &
/opt/noVNC/utils/launch.sh --vnc localhost:$VNC_PORT --listen $1 &
# Insecure option is needed to accept connections from the docker host.
vncserver $DISPLAY -depth $VNC_COL_DEPTH -geometry $VNC_RESOLUTION -SecurityTypes None -localhost no --I-KNOW-THIS-IS-INSECURE &
wait