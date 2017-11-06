#! /usr/bin/env python3

import subprocess
from pathlib import Path


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


def build_kernel(name, tag, extra_opts='', *, latest=False, squash=False):
    assert Path(name).is_dir()
    sq = '--squash' if squash else ''
    print_header(f'Building {name}' + ' (latest)' if latest else '')
    run(f'docker build -t lablup/kernel-{name}:{tag} {extra_opts} -f {name}/Dockerfile.{tag} {sq} {name}')
    if latest:
        run(f'docker tag lablup/kernel-{name}:{tag} lablup/kernel-{name}:latest')


def build_common(name, tag, extra_opts=''):
    print_header(f'Building common.{name}:{tag}')
    run(f'docker build -t lablup/common-{name}:{tag} {extra_opts} -f commons/Dockerfile.{name}.{tag} commons')



build_kernel('base', 'debian', latest=True)
build_kernel('base', 'alpine')

# Current python3 version: 3.6.3
build_kernel('base-python3-minimal', 'debian', squash=True, latest=True)
build_kernel('base-python3-wheels',  'alpine')
build_kernel('base-python3-minimal', 'alpine', squash=True)
build_kernel('python3',              'debian', squash=True, latest=True)

# Current python2 version: 2.7.14
# TODO: (kernel-runner update required) build_kernel('base-python2-minimal', 'debian', squash=True, latest=True)
# TODO: (kernel-runner update required) build_kernel('base-python2-wheels',  'alpine')
# TODO: (kernel-runner update required) build_kernel('base-python2-minimal', 'alpine', squash=True)
# TODO: (kernel-runner update required) build_kernel('python2',              'debian', squash=True, latest=True)

build_kernel('git',     'alpine',     latest=True)
build_kernel('c',       '11-alpine',  latest=True)
build_kernel('cpp',     '14-alpine',  latest=True)
build_kernel('java',    '9-alpine',   latest=True)
build_kernel('java',    '8-alpine')
build_kernel('rust',    '1.17-alpine', latest=True)
build_kernel('go',      '1.9-alpine', latest=True)
build_kernel('go',      '1.8-alpine')
build_kernel('haskell', 'ghc8.2-debian', latest=True)
build_kernel('lua',     '5.3-alpine', latest=True)
build_kernel('lua',     '5.2-alpine')
build_kernel('lua',     '5.1-alpine')
build_kernel('php',     '7-alpine',   latest=True)
# TODO: (not implemented) build_kernel('nodejs',  '8-alpine',   latest=True)
build_kernel('nodejs',  '6-alpine',   latest=True)
build_kernel('julia',   '0.6-debian', latest=True)
build_kernel('r',       '3.3-alpine', latest=True)
# TODO: (not modernized) build_kernel('octave',  '4.2-debian',   latest=True)
# TODO: (not implemented) build_kernel('swift',  'XX-alpine', latest=True)
# TODO: (not implemented) build_kernel('swift',  'XX-alpine')
# TODO: (not implemented) build_kernel('mono',   'XX-alpine', latest=True)
# TODO: (not implemented) build_kernel('mono',   'XX-alpine')

build_common('bazel', '0.7-debian')
build_common('cuda', 'cuda8.0-cudnn6.0')
# unused - build_common('glibc', 'alpine')
# unused - build_common('bazel', '0.7-alpine')

# Our TensorFlow currently depends on CUDA 8 + cuDNN 6
# (TODO: upgrade to CUDA 9 + cuDNN 7 for Volta GPUs)
build_common('tensorflow', '1.4')
build_common('tensorflow', '1.4-gpu')
build_common('tensorflow', '1.3')
build_common('tensorflow', '1.3-gpu')

build_kernel('python3-tensorflow', '1.4')
build_kernel('python3-tensorflow', '1.4-gpu', latest=True)
build_kernel('python3-tensorflow', '1.3')
build_kernel('python3-tensorflow', '1.3-gpu')
build_kernel('python3-caffe',      '1.0')
# TODO: (GPU not implemented) build_kernel('python3-caffe',      '1.0-gpu', latest=True)
# TODO: (not implemented) build_kernel('python3-caffe2',     '0.8')
# TODO: (not implemented) build_kernel('python3-caffe2',     '0.8-gpu', latest=True)
build_kernel('python3-torch',      '0.2')
build_kernel('python3-torch',      '0.2-gpu', latest=True)
# TODO (not modernized): build_kernel('python3-theano',     '0.2')
# TODO (not modernized): build_kernel('python3-theano',     '0.2-gpu', latest=True)
