#! /bin/sh
# reference: /bin/run.sh

SUEXEC="/opt/kernel/su-exec $USER_ID:$GROUP_ID"

rm /opt/noVNC/index.html
ln -s /opt/noVNC/redirect.html /opt/noVNC/index.html

rm -rf /tmp/.X*

"$SUEXEC" /usr/bin/vncserver -geometry 1600x1200 > /dev/null 2>&1

# The next line is executed by the kernel runner's service port handler.
# "$SUEXEC" /opt/noVNC/utils/launch.sh --listen 6080 --vnc localhost:5901 > /dev/null 2>&1 &
