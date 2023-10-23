#!/bin/bash
mkdir -p /home/work/.modular
echo 'export MODULAR_HOME="/opt/mojo"' >> /home/work/.bashrc
echo 'export PATH="/opt/mojo/pkg/packages.modular.com_mojo/bin:$PATH"' >> /home/work/.bashrc
source /opt/mojo/pkg/packages.modular.com_mojo/venv/bin/activate
source ~/.bashrc
