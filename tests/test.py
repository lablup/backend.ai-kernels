#! /usr/bin/env python3
from abc import abstractmethod
from distutils.version import LooseVersion
import docker
import os
import re
import shutil
from sorna.proto import generate_uuid
import time
import unittest
import urllib.parse
import zmq

_apparmor_profile_path = '/etc/apparmor.d/docker-ptrace'


class ImageTestBase(object):

    image_name = ''

    @abstractmethod
    def basic_success(self):
        '''
        Should return a pair of the input code and the expected output
        substring in stdout.
        '''
        yield None, None

    @abstractmethod
    def basic_failure(self):
        '''
        Should return a pair of the input code and the expected output pair of
        exception name and argument.
        '''
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
        docker_version = LooseVersion(self.docker.version()['Version'])
        if docker_version >= LooseVersion('1.10'):
            # We already have our own jail!
            security_opt = ['seccomp:unconfined']
        else:
            security_opt = ['apparmor:docker-ptrace'] \
                           if os.path.exists(_apparmor_profile_path) else []
        result = self.docker.create_container(self.image_name,
                                              name=container_name,
                                              ports=[(2001, 'tcp')],
                                              volumes=['/home/work'],
                                              host_config=self.docker.create_host_config(
                                                 mem_limit='128m',
                                                 memswap_limit=0,
                                                 security_opt=security_opt,
                                                 port_bindings={2001: ('127.0.0.1', 2001)},
                                                 binds={
                                                     self.work_dir: {
                                                        'bind': '/home/work',
                                                        'mode': 'rw'
                                                    },
                                                 }),
                                              tty=False)
        self.container_id = result['Id']
        self.docker.start(self.container_id)
        self.kernel_addr = 'tcp://{}:{}'.format(self.docker_host, 2001)
        time.sleep(0.1)  # prevent corruption of containers when killed immediately

    def tearDown(self):
        try:
            self.docker.kill(self.container_id)
        except docker.errors.NotFound:
            # The container might have been terminated during tests due to errors.
            pass
        else:
            self.docker.remove_container(self.container_id)
        shutil.rmtree(self.work_dir)

    def execute(self, cell_id, code):
        ctx = zmq.Context()
        ctx.setsockopt(zmq.LINGER, 50)
        cli = ctx.socket(zmq.REQ)
        cli.connect(self.kernel_addr)
        with cli:
            msg = ('{}'.format(cell_id).encode('ascii'), code.encode('utf8'))
            cli.send_multipart(msg)
            if cli.poll(timeout=3000) == 0:  # timeout in millisec
                raise TimeoutError('Container does not respond.')
            resp = cli.recv_json()
        ctx.destroy()
        return resp

    def test_basic_success(self):
        inner_exception = None
        for idx, case in enumerate(self.basic_success()):
            if len(case) == 2:
                code = case[0]
                expected_stdout = case[1]
                expected_stderr = None
            elif len(case) == 3:
                code = case[0]
                expected_stdout = case[1]
                expected_stderr = case[2]
            if code is None:
                continue
            with self.subTest(subcase=idx + 1):
                try:
                    resp = self.execute(idx, code)
                except TimeoutError as e:
                    # (Re-)raised exception here is captured by subTest ctxmgr.
                    # We just store the exception object and break out of the sub-case loop.
                    inner_exception = AssertionError('Timeout detected at sub-case {}'.format(idx + 1))
                    break
                self.assertIn('stdout', resp)
                self.assertIn('stderr', resp)
                self.assertIn('exceptions', resp)
                self.assertIsInstance(resp['stdout'], str)
                self.assertIn(expected_stdout, resp['stdout'])
                if expected_stderr:
                    self.assertIn(expected_stderr, resp['stderr'])
                else:
                    if not (resp['stderr'] is None or resp['stderr'] == ''):
                        self.fail('stderr is expected to be empty but is not: {!r}'
                                  .format(resp['stderr']))
        if inner_exception:
            self.fail(inner_exception)

    def test_basic_failure(self):
        inner_exception = None
        for idx, (code, expected) in enumerate(self.basic_failure()):
            if code is None:
                continue
            with self.subTest(subcase=idx + 1):
                try:
                    resp = self.execute(idx, code)
                except TimeoutError as e:
                    inner_exception = AssertionError('Timeout detected at sub-case {}'.format(idx + 1))
                    break
                self.assertIn('stdout', resp)
                self.assertIn('stderr', resp)
                self.assertIn('exceptions', resp)
                self.assertIs(type(resp['exceptions']), list)
                self.assertGreater(len(resp['exceptions']), 0)
                err_name, err_arg = expected
                self.assertRegex(resp['exceptions'][0][0], '^' + re.escape(err_name))
                if err_arg:
                    self.assertIn(err_arg, resp['exceptions'][0][1])
        if inner_exception:
            self.fail(inner_exception)


class Python2ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python2'

    def basic_success(self):
        yield 'print "hello world"', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint c', '3'

    def basic_failure(self):
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)


class Python3ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python3'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'

    def basic_failure(self):
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)


_py3_tf_example = '''
import tensorflow as tf
import numpy as np
x_data = np.random.rand(100).astype(np.float32)
y_data = x_data * 0.1 + 0.3
W = tf.Variable(tf.random_uniform([1], -1.0, 1.0))
b = tf.Variable(tf.zeros([1]))
y = W * x_data + b
loss = tf.reduce_mean(tf.square(y - y_data))
optimizer = tf.train.GradientDescentOptimizer(0.5)
train = optimizer.minimize(loss)
init = tf.initialize_all_variables()
sess = tf.Session()
sess.run(init)
for step in range(201):
    sess.run(train)
    if step % 20 == 0:
        print(step, sess.run(W), sess.run(b))
print('done')
'''

class Python3TensorFlowImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python3-tensorflow'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'
        yield _py3_tf_example, 'done'

    def basic_failure(self):
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)


_py3_caffe_example = '''
import numpy as np
import caffe
caffe.set_mode_cpu()
print('done')
'''

class Python3CaffeImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python3-caffe'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'
        yield _py3_caffe_example, 'done'

    def basic_failure(self):
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)



class R3ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-r3'

    def basic_success(self):
        yield 'cat("hello world\n")', 'hello world'
        yield 'a = 1;b = 2;c = a + b;cat(c, "\n")', '3'
        yield 'library("ggplot2"); cat("success\n")', 'success'

    def basic_failure(self):
        yield 'stop("my error")', ('simpleError', 'my error')
        yield 'some_undefined_func()', ('simpleError', 'could not find function "some_undefined_func"')
        # checks if environment is properly isolated.
        yield 'print(ctx)', ('simpleError', "object 'ctx' not found")


class PHP5ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-php5'

    def basic_success(self):
        yield 'echo "hello world";', 'hello world'
        yield '$a = 1; $b = 2; $c = $a + $b; echo "$c";', '3'
        yield 'echo isset($my_nonexistent_variable) ? "1" : "0";', '0'
        # checks if our internal REPL code is NOT exposed.
        yield 'echo isset($context) ? "1" : "0";', '0'
        yield 'global $context; echo isset($context) ? "1" : "0";', '0'

    def basic_failure(self):
        yield 'throw new Exception("asdf");', ('Exception', 'asdf')
        yield '$x = 0 / 0;', ('Division by zero', None)


class PHP7ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-php7'

    def basic_success(self):
        yield 'echo "hello world";', 'hello world'
        yield '$a = 1; $b = 2; $c = $a + $b; echo "$c";', '3'
        yield 'echo isset($my_nonexistent_variable) ? "1" : "0";', '0'
        # checks if our internal REPL code is NOT exposed.
        yield 'echo isset($context) ? "1" : "0";', '0'
        yield 'global $context; echo isset($context) ? "1" : "0";', '0'

    def basic_failure(self):
        yield 'throw new Exception("asdf");', ('Exception', 'asdf')
        yield '$x = 0 / 0;', ('Division by zero', None)


class Nodejs4ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-nodejs4'

    def basic_success(self):
        yield 'console.log("hello world");', 'hello world'
        yield 'var a = 1; var b = 2; var c = a + b; console.log(c);', '3'
        yield 'console.log(process.version);', 'v4.'
        yield 'setTimeout(() => { console.log("async"); }, 100);', 'async'

    def basic_failure(self):
        yield 'console.log(some_undef_var);', ('ReferenceError', None)


class GitImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-git'

    def basic_success(self):
        yield 'invalid-shell-command', '', 'not found'
        yield 'git init .', ''
        yield 'echo "test" > a.txt', ''
        yield 'cat a.txt', 'test'
        yield 'git add a.txt', ''
        yield 'git commit -m "first commit"', '', 'Please tell me who you are'
        yield 'git config user.name Tester', '', ''
        yield 'git config user.email test@lablup.com', '', ''
        yield 'git commit -m "first commit"', '', ''
        yield 'git log', 'first commit'

    def basic_failure(self):
        yield None, None


class JailTest(ImageTestBase, unittest.TestCase):

    image_name = 'kernel-python3'

    def basic_success(self):
        yield 'import os; os.chmod("/home/work", 700)', '', ''

        # Since Python 3.5.1, we need to allow getrandom() syscall.
        yield 'import random; print(random.randint(0, 0))', '0', ''

    def basic_failure(self):
        yield 'import os\nos.chmod("/home/sorna", 700)', ('PermissionError', None)
        yield 'import os\nos.chmod("../sorna", 700)', ('PermissionError', None)
        yield 'import os\nos.chmod("/home/work/../sorna", 700)', ('PermissionError', None)
        yield 'import os\nos.chmod("/home/work/./../sorna", 700)', ('PermissionError', None)
        yield 'import os\nos.chmod("/home/sorna/.", 700)', ('PermissionError', None)
        yield 'import os\nos.mkdir("/home/work/test")\n' + \
              'os.chmod("/home/work/test/../../sorna/", 700)', ('PermissionError', None)


if __name__ == '__main__':
    unittest.main()
