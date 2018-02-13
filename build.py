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


def build_kernel(name, tag, extra_opts='', *, latest=False, squash=False):
    assert Path(name).is_dir()

    sq = '--squash' if squash else ''
    latest_tag = 'latest'
    if 'dense' in tag:
        latest_tag += '-dense'
    if 'gpu' in tag:
        latest_tag += '-gpu'

    print_header(f'Building {name}' + ' (latest)' if latest else '')
    run('docker build '
        f'-t lablup/kernel-{name}:{tag} {extra_opts} '
        f'-f {name}/Dockerfile.{tag} {sq} {name}')
    if latest:
        run('docker tag '
            f'lablup/kernel-{name}:{tag} '
            f'lablup/kernel-{name}:{latest_tag}')
    if auto_push:
        run(f'docker push lablup/kernel-{name}:{tag}')
        if latest:
            run(f'docker push lablup/kernel-{name}:{latest_tag}')


def build_common(name, tag, extra_opts=''):
    print_header(f'Building common.{name}:{tag}')
    run('docker build '
        f'-t lablup/common-{name}:{tag} {extra_opts} '
        f'-f commons/Dockerfile.{name}.{tag} commons')
    if auto_push:
        run(f'docker push lablup/common-{name}:{tag}')



build_kernel('base', 'debian', latest=True)
build_kernel('base', 'alpine')
build_kernel('base', 'ubuntu', latest=True)

build_kernel('base-python-minimal', '3.6-debian', squash=True, latest=True)
build_kernel('base-python-wheels',  '3.6-alpine')
build_kernel('base-python-minimal', '3.6-alpine', squash=True)
build_kernel('base-python-minimal', '3.6-ubuntu', squash=True, latest=True)
build_kernel('python',              '3.6-debian', squash=True, latest=True)
build_kernel('python',              '3.6-ubuntu', squash=True, latest=True)

# TODO: (kernel-runner update required) build_kernel('base-python-minimal', '2.7-debian', squash=True, latest=True)
# TODO: (kernel-runner update required) build_kernel('base-python-wheels',  '2.7-alpine')
# TODO: (kernel-runner update required) build_kernel('base-python-minimal', '2.7-alpine', squash=True)
# TODO: (kernel-runner update required) build_kernel('python',              '2.7-debian', squash=True, latest=True)

build_kernel('git',     'alpine', squash=True, latest=True)
build_kernel('c',       'gcc6.3-alpine', latest=True)
build_kernel('cpp',     'gcc6.3-alpine', latest=True)
build_kernel('java',    '8-alpine',      latest=True)
build_kernel('rust',    '1.17-alpine',   latest=True)
build_kernel('go',      '1.9-alpine',    latest=True)
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


## Our TensorFlow currently depends on CUDA 8 + cuDNN 6
## (TODO: upgrade to CUDA 9 + cuDNN 7 for Volta GPUs)
#build_common('tensorflow', '1.5-py36', squash=True)
#build_common('tensorflow', '1.5-py36-gpu', squash=True)
#build_common('tensorflow', '1.4-py36', squash=True)
#build_common('tensorflow', '1.4-py36-gpu', squash=True)
#build_common('tensorflow', '1.3-py36', squash=True)
#build_common('tensorflow', '1.3-py36-gpu', squash=True)
#
#build_kernel('python-tensorflow', '1.5-py36', squash=True)
#build_kernel('python-tensorflow', '1.5-py36-gpu', squash=True, latest=True)
#build_kernel('python-tensorflow', '1.4-py36', squash=True)
#build_kernel('python-tensorflow', '1.4-py36-gpu', squash=True, latest=True)
#build_kernel('python-tensorflow', '1.3-py36', squash=True)
#build_kernel('python-tensorflow', '1.3-py36-gpu', squash=True)

build_kernel('python-caffe',      '1.0-py36', latest=True, squash=True)
# TODO: (GPU not implemented) build_kernel('python-caffe',      '1.0-py36-gpu', squash=True, latest=True)
# TODO: (not implemented) build_kernel('python-caffe2',     '0.8-py36', squash=True)
# TODO: (not implemented) build_kernel('python-caffe2',     '0.8-py36-gpu', squash=True, latest=True)
build_kernel('python-torch',      '0.2-py36', latest=True, squash=True)
build_kernel('python-torch',      '0.2-py36-gpu', latest=True, squash=True)
# TODO (not modernized): build_kernel('python-theano',     '0.2-py36', squash=True)
# TODO (not modernized): build_kernel('python-theano',     '0.2-py36-gpu', squash=True, latest=True)

# CNTK image (currently draft version base on Ubuntu:16.04)
build_kernel('python-cntk', '2.2-py36', latest=True, squash=True)

# Dense builds are for sharing each server with multiple tenants
# (with some patches that enforces resource restrictions)
build_common('tensorflow', '1.5-py36-dense')
build_common('tensorflow', '1.5-py36-dense-gpu')
build_common('tensorflow', '1.4-py36-dense')
build_common('tensorflow', '1.4-py36-dense-gpu')
build_common('tensorflow', '1.3-py36-dense')
build_common('tensorflow', '1.3-py36-dense-gpu')

build_kernel('python-tensorflow', '1.5-py36-dense', latest=True, squash=True)
build_kernel('python-tensorflow', '1.5-py36-dense-gpu', latest=True, squash=True)
build_kernel('python-tensorflow', '1.4-py36-dense', latest=True, squash=True)
build_kernel('python-tensorflow', '1.4-py36-dense-gpu', latest=True, squash=True)
build_kernel('python-tensorflow', '1.3-py36-dense', squash=True)
build_kernel('python-tensorflow', '1.3-py36-dense-gpu', squash=True)
