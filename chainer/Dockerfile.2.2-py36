# ref: https://docs.microsoft.com/en-us/cognitive-toolkit/Setup-Linux-Python?tabs=cntkpy22#tabpanel_FweSSiKxOx_cntkpy22
# ref: https://github.com/Microsoft/CNTK/blob/master/Tools/docker/CNTK-CPUOnly-Image/Dockerfile
FROM lablup/common-cuda:cuda9.0-cudnn7.1

ENV LANG=C.UTF-8

RUN pip install --upgrade pip==9.0.1 && \
    pip install setuptools==38.2.4 && \
    pip install cupy==2.2.0 chainer==3.2.0

# Below scripts are already executed in python/Dockerfile.3.6-ubuntu
# Install kernel-runner scripts package
# RUN pip install --no-cache-dir "backend.ai-kernel-runner[python]~=1.0.4"

COPY policy.yml /home/sorna/policy.yml

