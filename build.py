#! /usr/bin/env python3

from contextvars import ContextVar
import subprocess
import sys
from pathlib import Path

import click

auto_push = ContextVar('auto_push', default=False)


def run(shellcmd):
    return subprocess.run(shellcmd, shell=True, check=True)


def capture(shellcmd):
    return subprocess.run(shellcmd, shell=True, check=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)

def print_header(s):
    click.secho(s, fg='yellow', bold=True)


def build_kernel(name, tag, extra_opts='', *, squash=False):
    assert Path(name).is_dir()

    sq = '--squash' if squash else ''

    print_header(f'Building {name}')
    if name.startswith('vendor/'):
        short_name = name[len('vendor/'):]
    else:
        short_name = name
    dockerfile = Path(name) / f'Dockerfile.{tag}'
    if not dockerfile.is_file():
        click.secho(f'No dockerfile: {dockerfile}', fg='yellow', err=True)
        click.secho('Maybe you need to rename existing ones to follow '
                    'the new tag naming scheme.', fg='yellow', err=True)
        return
    dockerfile_txt = dockerfile.read_text(encoding='utf-8')
    if 'ai.backend.kernelspec' not in dockerfile_txt:
        click.secho(f'Dockerfile {dockerfile} is not updated for '
                    'the new kernelspec.',
                    fg='yellow', err=True)
        return
    run('docker build '
        f'-t lablup/{short_name}:{tag} {extra_opts} '
        f'-f {dockerfile} {sq} {name}')
    if auto_push.get():
        run(f'docker push lablup/{short_name}:{tag}')


def build_common(name, tag, extra_opts=''):
    print_header(f'Building common.{name}:{tag}')
    run('docker build '
        f'-t lablup/common-{name}:{tag} {extra_opts} '
        f'-f commons/Dockerfile.{name}.{tag} commons')
    if auto_push.get():
        run(f'docker push lablup/common-{name}:{tag}')


available_builds = [
    'python',
    'alpine-base', 'alpine-ext',
    'compute-base',
    'chainer',
    'past','ff',
    'tf-builder',
    'tf-pkg-old', 'tf-pkg-current', 'tf-pkg-future',
    'tf-old', 'tf-current', 'tf-future',
    'caffe', 'pytorch',
    'cntk-builder','cntk',
    'vendor-aws', 'vendor-ngc',
]


@click.command()
@click.option('-b', '--build', multiple=True, type=click.Choice(available_builds),
              help='Build the given bundle.')
@click.option('--list-builds', is_flag=True,
              help='Display all available bundles.')
@click.option('--auto-push', '_auto_push', is_flag=True,
              help='Automatically push to the Docker Hub after successful builds.')
def main(build, list_builds, _auto_push):
    if list_builds:
        for b in available_builds:
            print(b)
        return

    auto_push.set(_auto_push)

    if 'python' in build:
        build_kernel('base',    '3.6')
        build_kernel('base',    '3.6-ubuntu18.04')
        build_kernel('python',  '2.7-ubuntu18.04')
        build_kernel('python',  '3.6-ubuntu18.04')
        build_kernel('python',  '3.7-anaconda2018.12')

    if 'alpine-base' in build:
        build_kernel('git',     'alpine3.8')
        build_kernel('c',       'gcc6.3-alpine3.8')
        build_kernel('c',       'gcc6.3-alpine3.8-tester')
        build_kernel('cpp',     'gcc6.3-alpine3.8')
        build_kernel('java',    '8-alpine3.8')
        build_kernel('nodejs',  '10-alpine3.8')
        build_kernel('nodejs',  '12-alpine3.8')        
        build_kernel('lua',     '5.1-alpine3.8')
        build_kernel('lua',     '5.2-alpine3.8')
        build_kernel('lua',     '5.3-alpine3.8')

    if 'alpine-ext' in build:
        build_kernel('scala',    '2.12-alpine3.8')
        build_kernel('rust',    '1.17-alpine3.8')
        build_kernel('go',      '1.8-alpine3.8')
        build_kernel('go',      '1.9-alpine3.8')
        build_kernel('haskell', 'ghc8.2-debian')
        build_kernel('php',     '7-alpine3.8')
        build_kernel('scheme',  '9.2-alpine3.8')

    if 'compute-base' in build:
        build_kernel('julia',   '1.0-ubuntu18.04')
        build_kernel('r',       '3.5-ubuntu18.04')

    # TODO: (not modernized) build_kernel('octave',  '4.2-debian')
    # TODO: (not implemented) build_kernel('swift',  'XX-alpine')
    # TODO: (not implemented) build_kernel('swift',  'XX-alpine')
    # TODO: (not implemented) build_kernel('mono',   'XX-alpine')
    # TODO: (not implemented) build_kernel('mono',   'XX-alpine')

    if 'chainer' in build:
        build_kernel('python-chainer',   '1.0-py36')
        build_kernel('python-chainer',   '2.0-py36')
        build_kernel('python-chainer',   '3.0-py36')
        build_kernel('python-chainer',   '4.0-py36')
        build_kernel('python-chainer',   '5.0-py36')
        build_kernel('python-chainer',   '6.0-py36')
    
    if 'tf-builder' in build:
        build_common('bazel', '0.5-ubuntu16.04')
        build_common('bazel', '0.11-ubuntu16.04')
        build_common('bazel', '0.15-ubuntu16.04')
        build_common('bazel', '0.20-ubuntu16.04')
        build_common('numpy', '1.15-py36-ubuntu16.04-mkl2019.0')
        build_kernel('base', 'ubuntu16.04-mkl2018.3')
        build_kernel('base', 'ubuntu16.04-mkl2019')
        build_kernel('base', 'ubuntu16.04-mkl2019.1')       
        build_kernel('base', 'ubuntu16.04-mkl2019.2')        

    ## Our TensorFlow currently depends on CUDA 9.0 + cuDNN 7.1
    if 'tf-pkg-old' in build:
        build_common('tensorflow', '1.0-py36')
        build_common('tensorflow', '1.0-py36-cuda8')
        build_common('tensorflow', '1.1-py36')
        build_common('tensorflow', '1.1-py36-cuda8')
        build_common('tensorflow', '1.2-py36')
        build_common('tensorflow', '1.2-py36-cuda8')
        build_common('tensorflow', '1.3-py36')
        build_common('tensorflow', '1.3-py36-cuda8')
        build_common('tensorflow', '1.4-py36')
        build_common('tensorflow', '1.4-py36-cuda8')
        build_common('tensorflow', '1.5-py36')
        build_common('tensorflow', '1.5-py36-cuda9')
        build_common('tensorflow', '1.6-py36')
        build_common('tensorflow', '1.6-py36-cuda9')
        build_common('tensorflow', '1.7-py36')
        build_common('tensorflow', '1.7-py36-cuda9')

    if 'tf-pkg-current' in build:
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
        build_common('tensorflow', '1.13-py36')
        build_common('tensorflow', '1.13-py36-cuda9')
        build_common('tensorflow', '1.14-py36')
        build_common('tensorflow', '1.14-py36-cuda9')

    if 'tf-pkg-future' in build:
        build_common('tensorflow', '2.0-py36')
        build_common('tensorflow', '2.0-py36-cuda9')
        
    if 'tf-old' in build:
        build_kernel('python-tensorflow', '1.0-py36')
        build_kernel('python-tensorflow', '1.0-py36-cuda8')
        build_kernel('python-tensorflow', '1.1-py36')        
        build_kernel('python-tensorflow', '1.1-py36-cuda8')
        build_kernel('python-tensorflow', '1.2-py36')                
        build_kernel('python-tensorflow', '1.2-py36-cuda8')
        build_kernel('python-tensorflow', '1.3-py36')                        
        build_kernel('python-tensorflow', '1.3-py36-cuda8')
        build_kernel('python-tensorflow', '1.4-py36')
        build_kernel('python-tensorflow', '1.4-py36-cuda8')
        build_kernel('python-tensorflow', '1.5-py36')
        build_kernel('python-tensorflow', '1.5-py36-cuda9')
        build_kernel('python-tensorflow', '1.6-py36')
        build_kernel('python-tensorflow', '1.6-py36-cuda9')
        build_kernel('python-tensorflow', '1.7-py36')
        build_kernel('python-tensorflow', '1.7-py36-cuda9')

    if 'tf-current' in build:
        build_kernel('python-tensorflow', '1.8-py36')
        build_kernel('python-tensorflow', '1.8-py36-cuda9')
        build_kernel('python-tensorflow', '1.9-py36')
        build_kernel('python-tensorflow', '1.9-py36-cuda9')
        build_kernel('python-tensorflow', '1.10-py36')
        build_kernel('python-tensorflow', '1.10-py36-cuda9')
        build_kernel('python-tensorflow', '1.11-py36')
#        build_kernel('python-tensorflow', '1.11-py36-tpu')
#        build_kernel('python-tensorflow', '1.11-py36-srv')
        build_kernel('python-tensorflow', '1.11-py36-cuda9')
#        build_kernel('python-tensorflow', '1.11-py36-srv-cuda9')
        build_kernel('python-tensorflow', '1.12-py36')
#        build_kernel('python-tensorflow', '1.12-py36-tpu')
#        build_kernel('python-tensorflow', '1.12-py36-cuda9')
#        build_kernel('python-tensorflow', '1.12-py36-srv')
#        build_kernel('python-tensorflow', '1.12-py36-srv-cuda9')
        build_kernel('python-tensorflow', '1.13-py36')
#        build_kernel('python-tensorflow', '1.13-py36-tpu')
        build_kernel('python-tensorflow', '1.13-py36-cuda9')
#        build_kernel('python-tensorflow', '1.13-py36-srv')
#        build_kernel('python-tensorflow', '1.13-py36-srv-cuda9')        
        build_kernel('python-tensorflow', '1.14-py36')
        build_kernel('python-tensorflow', '1.14-py36-cuda9')

    if 'past' in build:
        build_common('tensorflow', 'ff.19.05-py36-cuda9')
        build_kernel('python-ff', '19.05-py36-cuda9')
        build_common('tensorflow', 'ff.19.06-py36-cuda9')
        build_kernel('python-ff', '19.06-py36-cuda9')
        
    if 'ff' in build:
        build_common('tensorflow', 'ff.19.06-py36')
        build_common('tensorflow', 'ff.19.06-py36-cuda9')
        build_kernel('python-ff', '19.06-py36')
        build_kernel('python-ff', '19.06-py36-cuda9')        

    if 'tf-future' in build:
        build_kernel('python-tensorflow', '2.0-py36')
        build_kernel('python-tensorflow', '2.0-py36-cuda9')

    if 'caffe' in build:
        build_kernel('python-caffe',  '1.0-py36')
        build_kernel('python-caffe2', '1.0-py36')

    if 'pytorch' in build:
        build_kernel('python-pytorch', '0.1-py36-cuda8')
        build_kernel('python-pytorch', '0.2-py36')
        build_kernel('python-pytorch', '0.2-py36-cuda8')
        build_kernel('python-pytorch', '0.3-py36')
        build_kernel('python-pytorch', '0.3-py36-cuda9')
        build_kernel('python-pytorch', '0.4-py36')
        build_kernel('python-pytorch', '0.4-py36-cuda9')
        build_kernel('python-pytorch', '1.0-py36')
        build_kernel('python-pytorch', '1.0-py36-cuda10')
        build_kernel('python-pytorch', '1.1-py36')
        build_kernel('python-pytorch', '1.1-py36-cuda10')

    # Python Theano
    # TODO (not modernized):
    #build_kernel('python-theano',     '0.2-py36')
    #build_kernel('python-theano',     '0.2-py36-gpu')

    # CNTK image (currently draft version base on Ubuntu:16.04)
    if 'cntk-builder' in build:
        build_common('cntk','2.0-py36')
        build_common('cntk','2.1-py36')
        build_common('cntk','2.2-py36')
        build_common('cntk','2.3-py36')
        build_common('cntk','2.4-py36')
        build_common('cntk','2.5-py36')
        build_common('cntk','2.5-py36-cuda9')        
        build_common('cntk','2.6-py36')
        build_common('cntk','2.6-py36-cuda9')        
        build_common('cntk','2.7-py36')
        build_common('cntk','2.7-py36-cuda9')        
    
    if 'cntk' in build:
        build_kernel('python-cntk', '2.0-py36')
        build_kernel('python-cntk', '2.1-py36')
        build_kernel('python-cntk', '2.2-py36')
        build_kernel('python-cntk', '2.3-py36')
        build_kernel('python-cntk', '2.4-py36')
        build_kernel('python-cntk', '2.5-py36')
        build_kernel('python-cntk', '2.5-py36-cuda9')        
        build_kernel('python-cntk', '2.6-py36')
        build_kernel('python-cntk', '2.6-py36-cuda9')        
        build_kernel('python-cntk', '2.7-py36')
        build_kernel('python-cntk', '2.7-py36-cuda9')        

    # AWS polly
    if 'vendor-aws' in build:
        build_kernel('vendor/aws_polly', '0.1-alpine3.8')

    if 'vendor-ngc' in build:
        build_kernel('vendor/ngc-caffe2', '18.08-py2')
        build_kernel('vendor/ngc-caffe3', '18.08-py3')

        build_kernel('vendor/ngc-tensorflow', '18.12-py3')
        build_kernel('vendor/ngc-tensorflow', '19.01-py3')
        build_kernel('vendor/ngc-tensorflow', '19.02-py3')        
        build_kernel('vendor/ngc-tensorflow', '19.03-py3')        
        build_kernel('vendor/ngc-tensorflow', '19.04-py3')
        build_kernel('vendor/ngc-tensorflow', '19.05-py3')                

        build_kernel('vendor/ngc-pytorch',  '18.12.1-py3')
        build_kernel('vendor/ngc-pytorch',  '19.01-py3')
        build_kernel('vendor/ngc-pytorch',  '19.02-py3')
        build_kernel('vendor/ngc-pytorch',  '19.03-py3')
        build_kernel('vendor/ngc-pytorch',  '19.04-py3')
        build_kernel('vendor/ngc-pytorch',  '19.05-py3')
        
        build_kernel('vendor/ngc-digits',  '18.12-tensorflow')
        build_kernel('vendor/ngc-digits',  '19.01-tensorflow')
        build_kernel('vendor/ngc-digits',  '19.02-tensorflow')
        build_kernel('vendor/ngc-digits',  '19.03-tensorflow')
        build_kernel('vendor/ngc-digits',  '19.04-tensorflow')
        build_kernel('vendor/ngc-digits',  '19.05-tensorflow')        
        
        build_kernel('vendor/ngc-digits',  '18.12-caffe')
        build_kernel('vendor/ngc-digits',  '19.01-caffe')
        build_kernel('vendor/ngc-digits',  '19.02-caffe')
        build_kernel('vendor/ngc-digits',  '19.03-caffe')
        build_kernel('vendor/ngc-digits',  '19.04-caffe')
        build_kernel('vendor/ngc-digits',  '19.05-caffe')        
        
        build_kernel('vendor/ngc-mxnet',  '19.04-mnxet')
        build_kernel('vendor/ngc-mxnet',  '19.05-mnxet')        

        build_kernel('vendor/ngc-NVCaffe', '19.04-NVCaffe')
        build_kernel('vendor/ngc-NVCaffe', '19.05-NVCaffe')        
        
        build_kernel('vendor/ngc-rapids',  '0.5-rapids-py3')
#        build_kernel('vendor/ngc-chainer', '4.0-py3')

if __name__ == '__main__':
    main()
