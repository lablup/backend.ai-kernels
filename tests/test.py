#! /usr/bin/env python3
from abc import abstractmethod
import docker
import os
import shutil
from sorna.proto import generate_uuid
import time
import unittest
import urllib.parse
import zmq


class ImageTestBase(object):

    image_name = ''

    @abstractmethod
    def basic_success(self):
        yield None, None

    @classmethod
    def setUpClass(cls):
        docker_args = docker.utils.kwargs_from_env()
        cls.docker = docker.Client(**docker_args)
        if 'base_url' in docker_args:
            cls.docker_host = urllib.parse.urlparse(docker_args['base_url']).hostname
        else:
            cls.docker_host = '127.0.0.1'
        if not cls.docker.images(cls.image_name):
            raise unittest.SkipTest('The image {} is not available. Build or import it first.'
                                    .format(cls.image_name))

    def setUp(self):
        container_name = 'kernel.test.{}'.format(generate_uuid())
        self.work_dir = os.path.join(os.getcwd(), '_workdir_{}'.format(container_name))
        os.makedirs(self.work_dir)
        result = self.docker.create_container(self.image_name,
                                              name=container_name,
                                              ports=[(2001, 'tcp')],
                                              volumes=['/home/work'],
                                              host_config=self.docker.create_host_config(
                                                 mem_limit='128m',
                                                 memswap_limit=0,
                                                 port_bindings={2001: ('0.0.0.0', )},
                                                 binds={
                                                     self.work_dir: {
                                                        'bind': '/home/work',
                                                        'mode': 'rw'
                                                    },
                                                 }),
                                              tty=False)
        self.container_id = result['Id']
        self.docker.start(self.container_id)
        container_info = self.docker.inspect_container(self.container_id)
        kernel_host_port = container_info['NetworkSettings']['Ports'] \
                                         ['2001/tcp'][0]['HostPort']
        self.kernel_addr = 'tcp://{}:{}'.format(self.docker_host, kernel_host_port)
        time.sleep(0.1)  # prevent corruption of containers when killed immediately

    def tearDown(self):
        try:
            self.docker.kill(self.container_id)
        except docker.errors.NotFound:
            # may have been terminated during tests
            pass
        else:
            self.docker.remove_container(self.container_id)
        shutil.rmtree(self.work_dir)

    def execute(self, cell_id, code):
        ctx = zmq.Context()
        ctx.setsockopt(zmq.LINGER, 50)
        cli = ctx.socket(zmq.REQ)
        cli.connect(self.kernel_addr)
        msg = ('{}'.format(cell_id).encode('ascii'), code.encode('utf8'))
        cli.send_multipart(msg)
        resp = cli.recv_json()
        cli.close()
        ctx.destroy()
        return resp

    def test_basic_success(self):
        for idx, (code, expected) in enumerate(self.basic_success()):
            with self.subTest(idx):
                resp = self.execute(idx, code)
                self.assertIn('stdout', resp)
                self.assertIn('stderr', resp)
                self.assertIn('exceptions', resp)
                self.assertIn(expected, resp['stdout'])


class Python27ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python27'

    def basic_success(self):
        yield 'print "hello world"', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint c', '3'


class Python34ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python34'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'


class PHP55ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-php55'

    def basic_success(self):
        yield 'echo "hello world";', 'hello world'
        yield '$a = 1; $b = 2; $c = $a + $b; echo "$c";', '3'


class Node34ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-node34'

    def basic_success(self):
        yield 'console.log("hello world");', 'hello world'
        yield 'var a = 1; var b = 2; var c = a + b; console.log(c);', '3'


if __name__ == '__main__':
    unittest.main()
