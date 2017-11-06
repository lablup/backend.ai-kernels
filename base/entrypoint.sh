#!/bin/sh

USER_ID=${LOCAL_USER_ID:-9001}
USER_NAME=work

echo "Starting with user $USER_NAME ($USER_ID)"
if [ -f /bin/ash ]; then  # for alpine
  useradd -s /bin/ash -d "/home/$USER_NAME" -M -r -u $USER_ID -U -o -c "User" $USER_NAME
else
  useradd -s /bin/bash -d "/home/$USER_NAME" -M -r -u $USER_ID -U -o -c "User" $USER_NAME
fi
chown -R work:work /home/sorna
sync

export HOME="/home/$USER_NAME"

exec su-exec $USER_NAME "$@"
