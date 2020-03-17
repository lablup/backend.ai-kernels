#! /bin/bash

echo "DEBUG DATETIME: $(date)"
echo ">> Setting up FreeCAD environment"

rm -rf /tmp/.X*

mkdir -p /home/work/Desktop
cat >/home/work/Desktop/FreeCAD.desktop <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=FreeCAD 0.16
Comment=
Exec=/opt/FreeCAD/bin/FreeCAD
Icon=/opt/FreeCAD/data/freecad-icon-64.png
Path=/home/work
Terminal=true
StartupNotify=false
EOF
chmod +x /home/work/Desktop/FreeCAD.desktop

echo ">> Starting background VNC server"
printf "backendai\nbackendai\n\n" | /opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID vncpasswd >/dev/null 2>&1

mkdir -p /home/work/.vnc
cat >/home/work/.vnc/xstartup <<EOF
#!/bin/sh

xhost +

# SESSION_MANAGER is inherited from the environment and some window
# managers require that it be cleared.
# http://osdir.com/ml/gnome.ximian.snapshots/2002-09/msg00034.html
# For example, Xfce4 version 4.6.1 and Deb5-64 require the unsetenv.
# Goolging indicates that others also require the unsetenv.
unsetenv SESSION_MANAGER

# Startup scripts, e.g. /etc/xdg/xfce4/xinitrc require
# http://en.wikipedia.org/wiki/D-Bus to run correctly.
unsetenv DBUS_SESSION_BUS_ADDRESS


# Set VNCSESSION to tell /etc/xdg/xfce4/xinitrc to not run xscreensaver
# http://vstone.eu/2009/04/disabling-xscreensaver-when-using-xfce-vnc/
setenv VNCSESSION yes

startxfce4 &

# Make sure that copy / paste are correctly forwarded to the VNC viewer
vncconfig -nowin &
EOF
chmod +x /home/work/.vnc/xstartup

cat >/home/work/.xscreensaver <<EOF
mode: off
EOF

chown -R $LOCAL_USER_ID:$LOCAL_GROUP_ID /home/work
/opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID /usr/bin/vncserver -geometry 1600x1200

echo ">> VNC server is started."

# The next line is executed by the kernel runner's service port handler.
# "$SUEXEC" /opt/noVNC/utils/launch.sh --listen 6080 --vnc localhost:5901 > /dev/null 2>&1 &
