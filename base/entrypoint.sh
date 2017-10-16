#!/bin/sh

# Add local user
# Either use the LOCAL_USER_ID if passed in at runtime or
# fallback

USER_ID=${LOCAL_USER_ID:-9001}
USER_NAME=work

echo "Starting with user $USER_NAME ($USER_ID)"
useradd -s /bin/ash -d "/home/$USER_NAME" -M -r -u $USER_ID -U -o -c "User" $USER_NAME
chown -R work:work /home/sorna
export HOME="/home/$USER_NAME"

exec su-exec $USER_NAME "$@"
