#!/bin/sh

USER_ID=${LOCAL_USER_ID:-9001}
USER_NAME=work

echo "Starting with user $USER_NAME ($USER_ID)"
useradd -s /bin/ash -d "/home/$USER_NAME" -M -r -u $USER_ID -U -o -c "User" $USER_NAME
chown -R work:work /home/sorna
export HOME="/home/$USER_NAME"

# To use libraries mounted at container creation
# e.g., nvidia driver and cuda libraries
ldconfig

exec su-exec $USER_NAME "$@"
