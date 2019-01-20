#! /usr/bin/env python3

import subprocess
from pathlib import Path

auto_push = False


def run(shellcmd):
    return subprocess.run(shellcmd, shell=True, check=True)


def capture(shellcmd):
    return subprocess.run(shellcmd, shell=True, check=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)

_hdr_color = capture('tput setaf 3').stdout.decode('ascii')
_hdr_color += capture('tput bold').stdout.decode('ascii')
_reset_color = capture('tput sgr0').stdout.decode('ascii')


def print_header(s):
    print(_hdr_color + s + _reset_color)


def build_kernel(name, tag, extra_opts='', *, squash=False):
    assert Path(name).is_dir()

    sq = '--squash' if squash else ''

    print_header(f'Building {name}')
    if name.startswith('vendor/'):
        short_name = name[len('vendor/'):]
    else:
        short_name = name
    run('docker build '
        f'-t lablup/kernel-{short_name}:{tag} {extra_opts} '
        f'-f {name}/Dockerfile.{tag} {sq} {name}')
    if auto_push:
        run(f'docker push lablup/kernel-{short_name}:{tag}')


def build_common(name, tag, extra_opts=''):
    print_header(f'Building common.{name}:{tag}')
    run('docker build '
        f'-t lablup/common-{name}:{tag} {extra_opts} '
        f'-f commons/Dockerfile.{name}.{tag} commons')
    if auto_push:
        run(f'docker push lablup/common-{name}:{tag}')


build_kernel('base', 'ubuntu16.04-mkl2018.3')
build_kernel('base', 'ubuntu16.04-mkl2019')

build_kernel('python',  '3.6-ubuntu18.04', squash=True)
build_kernel('python',  '3.7-anaconda2018.12', squash=True)

build_kernel('git',     'alpine', squash=True)
build_kernel('c',       'gcc6.3-alpine', squash=True)
build_kernel('c',       'gcc6.3-alpine-tester', squash=True)
build_kernel('cpp',     'gcc6.3-alpine', squash=True)
build_kernel('java',    '8-alpine', squash=True)
build_kernel('scala',    '2.12-alpine', squash=True)
build_kernel('rust',    '1.17-alpine', squash=True)
build_kernel('go',      '1.8-alpine', squash=True)
build_kernel('go',      '1.9-alpine', squash=True)
build_kernel('haskell', 'ghc8.2-debian')
build_kernel('lua',     '5.3-alpine', squash=True)
build_kernel('php',     '7-alpine', squash=True)
build_kernel('nodejs',  '6-alpine', squash=True)
build_kernel('nodejs',  '8-alpine', squash=True)
build_kernel('nodejs',  '10-alpine', squash=True)
build_kernel('julia',   '0.6-alpine', squash=True)
build_kernel('r',       '3.3-alpine', squash=True)
build_kernel('scheme',  '9.2-alpine', squash=True)
# TODO: (not modernized) build_kernel('octave',  '4.2-debian')
# TODO: (not implemented) build_kernel('swift',  'XX-alpine')
# TODO: (not implemented) build_kernel('swift',  'XX-alpine')
# TODO: (not implemented) build_kernel('mono',   'XX-alpine')
# TODO: (not implemented) build_kernel('mono',   'XX-alpine')


build_common('bazel', '0.5-ubuntu')
build_common('bazel', '0.11-ubuntu')
build_common('bazel', '0.15-ubuntu')
build_common('bazel', '0.20-ubuntu')
build_common('numpy', '1.15-py36-ubuntu16.04-mkl2019.0')
# unused - build_common('glibc', 'alpine')
# unused - build_common('bazel', '0.7-alpine')


## Our TensorFlow currently depends on CUDA 9.0 + cuDNN 7.1
build_common('tensorflow', '1.0-py36-cuda8')
build_common('tensorflow', '1.1-py36-cuda8')
build_common('tensorflow', '1.2-py36-cuda8')
build_common('tensorflow', '1.3-py36-cuda8')
build_common('tensorflow', '1.4-py36-cuda8')
build_common('tensorflow', '1.5-py36')
build_common('tensorflow', '1.5-py36-cuda9')
build_common('tensorflow', '1.6-py36')
build_common('tensorflow', '1.6-py36-cuda9')
build_common('tensorflow', '1.7-py36')
build_common('tensorflow', '1.7-py36-cuda9')
build_common('tensorflow', '1.8-py36')
build_common('tensorflow', '1.8-py36-cuda9')
build_common('tensorflow', '1.9-py36')
build_common('tensorflow', '1.9-py36-cuda9')
build_common('tensorflow', '1.10-py36')
build_common('tensorflow', '1.10-py36-cuda9')
build_common('tensorflow', '1.11-py36')
build_common('tensorflow', '1.11-py36-cuda9')
build_common('tensorflow', '1.12-py36')
build_common('tensorflow', '1.12-py36-cuda9')
build_common('tensorflow', '2.0-py36')
build_common('tensorflow', '2.0-py36-cuda9')

#
build_kernel('python-tensorflow', '1.0-py36-cuda8', squash=True)
build_kernel('python-tensorflow', '1.1-py36-cuda8', squash=True)
build_kernel('python-tensorflow', '1.2-py36-cuda8', squash=True)
build_kernel('python-tensorflow', '1.3-py36-cuda8', squash=True)
build_kernel('python-tensorflow', '1.4-py36-cuda8', squash=True)
build_kernel('python-tensorflow', '1.5-py36', squash=True)
build_kernel('python-tensorflow', '1.5-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.6-py36', squash=True)
build_kernel('python-tensorflow', '1.6-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.7-py36', squash=True)
build_kernel('python-tensorflow', '1.7-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.8-py36', squash=True)
build_kernel('python-tensorflow', '1.8-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.9-py36', squash=True)
build_kernel('python-tensorflow', '1.9-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.10-py36', squash=True)
build_kernel('python-tensorflow', '1.10-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.11-py36', squash=True)
build_kernel('python-tensorflow', '1.11-py36-tpu', squash=True)
build_kernel('python-tensorflow', '1.11-py36-srv', squash=True)
build_kernel('python-tensorflow', '1.11-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.11-py36-srv-cuda9', squash=True)
build_kernel('python-tensorflow', '1.12-py36', squash=True)
build_kernel('python-tensorflow', '1.12-py36-tpu', squash=True)
build_kernel('python-tensorflow', '1.12-py36-srv', squash=True)
build_kernel('python-tensorflow', '1.12-py36-cuda9', squash=True)
build_kernel('python-tensorflow', '1.12-py36-srv-cuda9', squash=True)
build_kernel('python-tensorflow', '2.0-py36', squash=True)
build_kernel('python-tensorflow', '2.0-py36-cuda9', squash=True)

# Python Caffe
build_kernel('python-caffe',      '1.0-py36', squash=True)

# Python Caffe2
build_kernel('python-caffe2',      '1.0-py36', squash=True)

# Python Pytorch
build_kernel('python-pytorch',      '0.1-py36-cuda8', squash=True)
build_kernel('python-pytorch',      '0.2-py36', squash=True)
build_kernel('python-pytorch',      '0.2-py36-cuda8', squash=True)
build_kernel('python-pytorch',      '0.3-py36', squash=True)
build_kernel('python-pytorch',      '0.3-py36-cuda9', squash=True)
build_kernel('python-pytorch',      '0.4-py36', squash=True)
build_kernel('python-pytorch',      '0.4-py36-cuda9', squash=True)
build_kernel('python-pytorch',      '1.0-py36', squash=True)
build_kernel('python-pytorch',      '1.0-py36-cuda9', squash=True)

# Python Theano
# TODO (not modernized):
#build_kernel('python-theano',     '0.2-py36', squash=True)
#build_kernel('python-theano',     '0.2-py36-gpu', squash=True)

# CNTK image (currently draft version base on Ubuntu:16.04)
build_kernel('python-cntk', '2.2-py36', squash=True)

# AWS polly
build_kernel('vendor/aws_polly', 'ubuntu', squash=True)
build_kernel('vendor/ngc-tensorflow', '18.12-py3', squash=True)
build_kernel('vendor/ngc-pytorch', '18.12.1-py3', squash=True)
build_kernel('vendor/ngc-digits', '18.12-tensorflow', squash=True)
