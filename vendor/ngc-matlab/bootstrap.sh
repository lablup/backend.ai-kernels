#! /bin/sh
# reference: /bin/run.sh

echo ">> Setting up MATLAB environment"
export USAGE=cloud
export VARIANTmatlab=matlabLNU

mkdir -p /home/work/Desktop
cat >/home/work/Desktop/MATLAB.desktop <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=MATLAB R2019b
Comment=
Exec=/opt/matlab/R2019b/bin/matlab
Icon=/opt/matlab/R2019b/bin/glnxa64/cef_resources/matlab_icon.png
Path=
Terminal=true
StartupNotify=false
Path=/home/matlab/Documents/MATLAB
EOF
chmod +x /home/work/Desktop/MATLAB.desktop

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
