#!/bin/bash
# Backend.AI image bootstrap for the rocker/rstudio-based r-base images.
#
# It is sourced as root by /opt/kernel/entrypoint.sh after the `work` account
# (uid=$LOCAL_USER_ID) has been created and before the kernel runner starts.

# R's site-library is group-writable by `staff`; the base only adds its own
# default user to it, so add the Backend.AI user here.
if id -u work > /dev/null 2>&1; then
    usermod -aG staff work
fi

mkdir -p /home/work/.rstudio-server
chown "${LOCAL_USER_ID:-1000}:${LOCAL_GROUP_ID:-1000}" /home/work/.rstudio-server

# Hand over to the rocker s6 supervision tree: it runs /etc/cont-init.d/*
# (01_set_env, 02_userconf) and then supervises rserver on port 8787.
/init &
