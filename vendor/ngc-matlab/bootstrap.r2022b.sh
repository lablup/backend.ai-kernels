#! /bin/sh
# Reference: /bin/run.sh
set -e

echo ">> Setting up MATLAB environment"
echo ""
echo "# == Matlab" >> ~/.bashrc
export USAGE=cloud && echo "export USAGE=cloud" >> ~/.bashrc
export VARIANTmatlab=matlabLNU && echo "export VARIANTmatlab=matlabLNU" >> ~/.bashrc
export SILENT=false && echo "export SILENT=false" >> ~/.bashrc

SHM_SIZE=$( df -P | grep shm | sed -n -e 's/^\w*\s*\([0-9]*\)\s.*/\1/p' )
if [ -z "$SILENT" ] && [ -n "$SHM_SIZE" ] ; then
    # Check it is big enough
    if [ ${SHM_SIZE} -le 524200 ] ; then
        echo
        echo "WARNING:"
        echo
        echo "This container has a shared area (/dev/shm) of size ${SHM_SIZE}kB. The MATLAB "
        echo "desktop requires at least 512MB to run correctly. Restart the session with "
        echo "512M of the shared memory."
        echo
    fi
fi

# Create a desktop launcher
mkdir -p /home/work/Desktop
cat >/home/work/Desktop/MATLAB.desktop <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=MATLAB R2022b
Comment=
Exec=/opt/matlab/R2022b/bin/matlab
Icon=/opt/matlab/R2022b/bin/glnxa64/cef_resources/matlab_icon.png
Path=
Terminal=true
StartupNotify=false
Path=/home/matlab/Documents/MATLAB
EOF
chmod +x /home/work/Desktop/MATLAB.desktop

# Make browser VNC auto connect
echo ">> Setting up noVNC auto-connection"
rm /opt/noVNC/index.html
ln -s /opt/noVNC/redirect.html /opt/noVNC/index.html
sed -i 's/password=matlab/password=backendai/' /opt/noVNC/index.html

# Clean up VNC lock files in case they exist
rm -rf /tmp/.X*

echo ">> Starting background VNC server"
printf "backendai\nbackendai\n\n" | /opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID vncpasswd >/dev/null 2>&1

cp -R /home/matlab/.config /home/work
chown -R $LOCAL_USER_ID:$LOCAL_GROUP_ID /home/work/.config
cp -R /home/matlab/Documents /home/work
chown -R $LOCAL_USER_ID:$LOCAL_GROUP_ID /home/work/Documents
mkdir -p /home/work/.vnc
cp /home/matlab/.vnc/xstartup /home/work/.vnc/xstartup
/opt/kernel/su-exec $LOCAL_USER_ID:$LOCAL_GROUP_ID /usr/bin/vncserver -geometry 1600x1200
cat >/home/work/.xscreensaver <<EOF
mode: off
EOF

echo ">> VNC server is started."

# The next line will be executed by the kernel runner's service def.
# "$SUEXEC" /opt/noVNC/utils/launch.sh --listen 6080 --vnc localhost:5901 > /dev/null 2>&1 &
