#!/bin/bash
#systemd-run /usr/lib/rstudio-server/bin/rserver --server-daemonize=0 --www-port 8886
mkdir -p /home/work/.rstudio-server
#/usr/lib/rstudio-server/bin/rserver --server-daemonize 0 --auth-none 1 --www-port 8886 --server-user work
#/usr/lib/rstudio-server/bin/rstudio-server start
/init &