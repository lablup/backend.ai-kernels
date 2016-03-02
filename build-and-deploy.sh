#! /usr/bin/env python3

import argparse
from datetime import datetime
import os
from os import path
import subprocess
import time
import docker


def init_docker():
    docker_args = docker.utils.kwargs_from_env()
    return docker.Client(timeout=3, **docker_args)


def is_fresh(docker_cli, target):
    kernel_name = 'kernel-{}'.format(target)
    dump_name = kernel_name + '.gz'
    if path.exists(dump_name):
        target_images = list(filter(lambda item: len(item['RepoTags']) > 0
                                                 and item['RepoTags'][0].startswith(kernel_name + ':'),
                                    docker_cli.images()))
        if len(target_images) == 0:
            return True
        dump_mtime = datetime.fromtimestamp(path.getmtime(dump_name))
        return all(map(lambda item: datetime.fromtimestamp(int(item['Created'])) > dump_mtime,
                   target_images))
    else:
        return True


run = lambda cmd, **kwargs: subprocess.run(cmd, shell=True, check=True, **kwargs)
silent_run = lambda cmd, **kwargs: subprocess.run(cmd, shell=True, check=True,
                                                  stdout=subprocess.DEVNULL,
                                                  stderr=subprocess.DEVNULL, **kwargs)


def build(docker_cli, target):
    print('Rebuilding...')
    silent_run('docker build -t kernel-{0} {0}'.format(target))
    print('Dumping to gzip-compressed file...')
    silent_run('docker save kernel-{0} | gzip > kernel-{0}.gz'.format(target))
    print('Done.')


def deploy(docker_cli, target):
    kernel_name = 'kernel-{}'.format(target)
    dump_name = 'kernel-{}.gz'.format(target)
    ssh_key = os.getenv('SORNA_SSH_KEY', '~/sorna.pem')
    host    = os.getenv('SORNA_HOST', 'ubuntu@sorna.lablup')
    print('Uploading the dump...')
    run('scp -i {0} ./{2} {1}:~'.format(ssh_key, host, dump_name))
    remote_cmds = [
        'zcat {0} | docker load'.format(dump_name),
        'rm {0}'.format(dump_name),
        'docker tag -f {0} docker-registry.lablup/{0}'.format(kernel_name),
        'docker push docker-registry.lablup/{0}'.format(kernel_name),
    ]
    for cmd in remote_cmds:
        run("ssh -i {0} {1} '{2}'".format(ssh_key, host, cmd))
    print('Done.')

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('targets', nargs='+', help='The image names to build and deploy.')
    args = parser.parse_args()

    docker_cli = init_docker()

    for target in args.targets:
        if is_fresh(docker_cli, target):
            print('Target {} requires rebuilding.'.format(target))
            build(docker_cli, target)
        else:
            print('Skipping rebuild of latest target image for {}.'.format(target))
        deploy(docker_cli, target)
