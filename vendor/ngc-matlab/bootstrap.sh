#! /bin/sh
# reference: /bin/run.sh

echo "Setting up noVNC auto-connection"
rm /opt/noVNC/index.html
ln -s /opt/noVNC/redirect.html /opt/noVNC/index.html

rm -rf /tmp/.X*

echo "Starting background VNC server"
export PASSWORD=matlab
printf "${PASSWORD}\n${PASSWORD}\n\n" | /opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID vncpasswd

ls -al /home/work
/opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID /usr/bin/vncserver -geometry 1600x1200

echo "VNC server is started."

# The next line is executed by the kernel runner's service port handler.
# "$SUEXEC" /opt/noVNC/utils/launch.sh --listen 6080 --vnc localhost:5901 > /dev/null 2>&1 &
