#!/bin/ash

# Add local user
# Either use the LOCAL_USER_ID if passed in at runtime or
# fallback

USER_ID=${LOCAL_USER_ID:-9001}
USER_NAME=work

echo "Starting with UID : $USER_ID"
useradd --shell /bin/ash -u $USER_ID -o -c "" -m $USER_NAME
export HOME="/home/$USER_NAME"

chown -R work:work /home/sorna

exec su-exec $USER_NAME "$@"
