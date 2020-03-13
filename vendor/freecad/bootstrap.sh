#! /bin/bash

echo "DEBUG DATETIME: $(date)"
echo ">> Setting up FreeCAD environment"
#export USAGE=cloud
#export VARIANTmatlab=matlabLNU

mkdir -p /home/work/Desktop
cat >/home/work/Desktop/FreeCAD.desktop <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=FreeCAD 0.16
Comment=
Exec=freecad-git
Path=/home/work
Terminal=true
StartupNotify=false
EOF
chmod +x /home/work/Desktop/FreeCAD.desktop

echo ">> Setting up noVNC auto-connection"
rm /opt/noVNC/index.html
ln -s /opt/noVNC/redirect.html /opt/noVNC/index.html

rm -rf /tmp/.X*

echo ">> Starting background VNC server"
printf "backendai\nbackendai\n\n" | /opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID vncpasswd >/dev/null 2>&1

cp -R /home/matlab/.config /home/work
cp -R /home/matlab/Documents /home/work
cp /home/matlab/.vnc/xstartup /home/work/.vnc/xstartup
chown -R $LOCAL_USER_ID:$LOCAL_GROUP_ID /home/work
/opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID /usr/bin/vncserver -geometry 1600x1200
cat >/home/work/.xscreensaver <<EOF
mode: off
EOF

echo ">> VNC server is started."

# The next line is executed by the kernel runner's service port handler.
# "$SUEXEC" /opt/noVNC/utils/launch.sh --listen 6080 --vnc localhost:5901 > /dev/null 2>&1 &
