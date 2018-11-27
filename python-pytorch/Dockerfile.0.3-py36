FROM lablup/kernel-python:3.6-ubuntu

# Install Pytorch
RUN pip install --no-cache-dir \
        http://download.pytorch.org/whl/cu90/torch-0.3.0.post4-cp36-cp36m-linux_x86_64.whl \
	torchvision && \
    rm -rf /root/.cache

# vim: ft=dockerfile sts=4 sw=4 et tw=0
