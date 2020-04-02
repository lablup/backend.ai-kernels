#!/bin/bash

if [ -n "$VNC_PASSWORD" ]; then
    echo -n "$VNC_PASSWORD" > /.password1
    x11vnc -storepasswd $(cat /.password1) /.password2
    chmod 400 /.password*
    sed -i 's/^command=x11vnc.*/& -rfbauth \/.password2/' /etc/supervisor/conf.d/supervisord.conf
    export VNC_PASSWORD=
fi

if [ -n "$X11VNC_ARGS" ]; then
    sed -i "s/^command=x11vnc.*/& ${X11VNC_ARGS}/" /etc/supervisor/conf.d/supervisord.conf
fi

if [ -n "$OPENBOX_ARGS" ]; then
    sed -i "s#^command=/usr/bin/openbox.*#& ${OPENBOX_ARGS}#" /etc/supervisor/conf.d/supervisord.conf
fi

if [ -n "$RESOLUTION" ]; then
    sed -i "s/1024x768/$RESOLUTION/" /usr/local/bin/xvfb.sh
fi

#USER=${USER:-root}
USER=work
HOME=/home/work
cp -r /root/{.gtkrc-2.0,.asoundrc} ${HOME}
#[ -d "/dev/snd" ] && chgrp -R adm /dev/snd
sed -i -e "s|%USER%|$USER|" -e "s|%HOME%|$HOME|" /etc/supervisor/conf.d/supervisord.conf

# home folder 
if [ ! -x "$HOME/.config/pcmanfm/LXDE/" ]; then
    mkdir -p $HOME/.config/pcmanfm/LXDE/
    ln -sf /usr/local/share/doro-lxde-wallpapers/desktop-items-0.conf $HOME/.config/pcmanfm/LXDE/
    chown -R $USER:$USER $HOME
fi

# nginx workers
sed -i 's|worker_processes .*|worker_processes 1;|' /etc/nginx/nginx.conf

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
setenv DISPLAY :1
gnome-session &
gnome-panel &
gnome-settings-daemon &
metacity &
nautilus &
# Make sure that copy / paste are correctly forwarded to the VNC viewer
vncconfig -nowin &
EOF
chmod +x /home/work/.vnc/xstartup

mkdir -p $HOME/.config/nautilus
chown -R $USER:$USER $HOME/.config/nautilus 
mkdir -p $HOME/.gtk-bookmarks
chown -R $USER:$USER $HOME/.gtk-bookmarks 

cat >/home/work/.xscreensaver <<EOF
mode: off
EOF

chown -R $LOCAL_USER_ID:$LOCAL_GROUP_ID /home/work
#/opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID /usr/bin/vncserver -geometry 1600x1200

#echo ">> VNC server is started."

supervisord -c /etc/supervisor/supervisord.conf
