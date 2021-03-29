#!/bin/bash
# Let self SSH possible
cp -rp /home/work/id_container /home/work/.ssh/id_rsa
cat /home/work/.ssh/authorized_keys > /home/work/.ssh/id_rsa.pub
chown work.work /home/work/.ssh/id_rsa.pub
chmod 644 /home/work/.ssh/id_rsa.pub

