#!/bin/bash

echo $(whoami) >> /tmp/whoami
echo $(id) >> /tmp/id_

# Update the user and group IDs based on environment variables
groupmod -g ${USER_GID} work
usermod -u ${USER_UID} -g ${USER_GID} work

# Set up SSH keys based on environment variables
if [[ ! -z "${SSH_PRIVATE_KEY}" ]]; then
    echo "${SSH_PRIVATE_KEY}" > /home/work/.ssh/id_rsa
    chmod 600 /home/work/.ssh/id_rsa
fi
if [[ ! -z "${SSH_PUBLIC_KEY}" ]]; then
    echo "${SSH_PUBLIC_KEY}" > /home/work/.ssh/id_rsa.pub
    cat /home/work/.ssh/id_rsa.pub >> /home/work/.ssh/authorized_keys
fi

# Update the ownership of the home directory and SSH keys
chown -R work:work /home/work /home/work/.ssh

# Start the SSH server
exec /usr/sbin/sshd -D
