#! /usr/bin/env python3
from abc import abstractmethod
import codecs
from distutils.version import LooseVersion
import io
import json
import os
import re
import shutil
import time
import unittest
import urllib.parse

import docker
import requests
import zmq

from ai.backend.common.utils import generate_uuid

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
        cls.docker = docker.APIClient()
        cls.docker_host = '127.0.0.1'
        if not cls.docker.get_image(cls.image_name):
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

        ret = self.docker.inspect_image(self.image_name)
        mem_limit    = ret['ContainerConfig']['Labels'].get('io.sorna.maxmem', '128m')
        exec_timeout = int(ret['ContainerConfig']['Labels'].get('io.sorna.timeout', '10'))
        max_cores    = int(ret['ContainerConfig']['Labels'].get('io.sorna.maxcores', '1'))
        envs_corecount = ret['ContainerConfig']['Labels'].get('io.sorna.envs.corecount', '')
        envs_corecount = envs_corecount.split(',') if envs_corecount else []

        num_cores = min(os.cpu_count(), max_cores)
        cores = '0-{}'.format(num_cores - 1)
        binds = {self.work_dir: {'bind': '/home/work', 'mode': 'rw'}}
        volumes = ['/home/work']
        devices = []
        envs = {k: str(num_cores) for k in envs_corecount}

        if 'yes' == ret['ContainerConfig']['Labels'].get('io.sorna.nvidia.enabled', 'no'):
            extra_binds, extra_devices = self.prepare_nvidia()
            binds.update(extra_binds)
            devices.extend(extra_devices)
        result = self.docker.create_container(
            self.image_name,
            name=container_name,
            ports=[
                (2000, 'tcp'),
                (2001, 'tcp'),
                (2002, 'tcp'),
                (2003, 'tcp'),
            ],
            environment=['{}={}'.format(k, v) for k, v in envs.items()],
            volumes=volumes,
            host_config=self.docker.create_host_config(
                cpuset_cpus=cores,
                mem_limit=mem_limit,
                memswap_limit=0,
                security_opt=security_opt,
                port_bindings={
                    2000: ('127.0.0.1', 2000),
                    2001: ('127.0.0.1', 2001),
                    2002: ('127.0.0.1', 2002),
                    2003: ('127.0.0.1', 2003),
                },
                devices=devices,
                binds=binds,
            ),
            tty=False)
        self.container_id = result['Id']
        self.docker.start(self.container_id)
        time.sleep(0.1)  # prevent corruption of containers when killed immediately

    def tearDown(self):
        try:
            print('\n==== Container logs ====')
            print(self.docker.logs(self.container_id).decode('utf-8'))
            print('====\n')
            self.docker.kill(self.container_id)
        except docker.errors.NotFound:
            # The container might have been terminated during tests due to errors.
            pass
        else:
            self.docker.remove_container(self.container_id)
        shutil.rmtree(self.work_dir)

    def prepare_nvidia(self):
        r = requests.get('http://localhost:3476/docker/cli/json')
        nvidia_params = r.json()
        existing_volumes = set(vol['Name'] for vol in self.docker.volumes()['Volumes'])
        required_volumes = set(vol.split(':')[0] for vol in nvidia_params['Volumes'])
        missing_volumes = required_volumes - existing_volumes
        binds = {}
        for vol_name in missing_volumes:
            for vol_param in nvidia_params['Volumes']:
                if vol_param.startswith(vol_name + ':'):
                    _, _, permission = vol_param.split(':')
                    driver = nvidia_params['VolumeDriver']
                    self.docker.create_volume(name=vol_name, driver=driver)
        for vol_name in required_volumes:
            for vol_param in nvidia_params['Volumes']:
                if vol_param.startswith(vol_name + ':'):
                    _, mount_pt, permission = vol_param.split(':')
                    binds[vol_name] = {
                        'bind': mount_pt,
                        'mode': permission,
                    }
        devices = ['{0}:{0}:rwm'.format(dev) for dev in nvidia_params['Devices']]
        return binds, devices

    def execute(self, code, user_input=None):
        ctx = zmq.Context()
        ctx.setsockopt(zmq.LINGER, 50)
        kin = ctx.socket(zmq.PUSH)
        kin.connect(f'tcp://{self.docker_host}:2000')
        kout = ctx.socket(zmq.PULL)
        kout.connect(f'tcp://{self.docker_host}:2001')
        stdout = io.StringIO()
        stderr = io.StringIO()
        log = []
        media = []
        html = io.StringIO()
        decoders = (
            codecs.getincrementaldecoder('utf8')(),
            codecs.getincrementaldecoder('utf8')(),
        )
        try:
            msg_in = (b'code', code.encode('utf8'))
            kin.send_multipart(msg_in)
            while True:
                if kout.poll(timeout=10_000) == 0:  # 10 sec
                    raise TimeoutError('Container does not respond.')
                reply_type, reply_data = kout.recv_multipart()
                if reply_type == b'finished':
                    decoders[0].decode(b'', True)
                    decoders[1].decode(b'', True)
                    break
                elif reply_type == b'waiting-input':
                    assert isinstance(user_input, str), 'User input required'
                    msg_in = (b'code', user_input.encode('utf8'))
                    time.sleep(0.1)
                    kin.send_multipart(msg_in)
                elif reply_type == b'stdout':
                    stdout.write(decoders[0].decode(reply_data))
                elif reply_type == b'stderr':
                    stderr.write(decoders[1].decode(reply_data))
                elif reply_type == b'log':
                    log.append(json.loads(reply_data.decode()))
                elif reply_type == b'media':
                    media.append(json.loads(reply_data.decode()))
                elif reply_type == b'html':
                    html.write(reply_data.decode())
        finally:
            resp = {
                'stdout': stdout.getvalue(),
                'stderr': stderr.getvalue(),
                'log': log,
                'media': media,
                'html': html.getvalue(),
            }
            stdout.close()
            stderr.close()
            html.close()
            kin.close()
            kout.close()
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
                    resp = self.execute(code)
                except TimeoutError as e:
                    # (Re-)raised exception here is captured by subTest ctxmgr.
                    # We just store the exception object and break out of the sub-case loop.
                    inner_exception = AssertionError('Timeout detected at sub-case {}'.format(idx + 1))
                    break
                self.assertIn('stdout', resp)
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
                    resp = self.execute(code)
                except TimeoutError as e:
                    inner_exception = AssertionError('Timeout detected at sub-case {}'.format(idx + 1))
                    break
                self.assertIn('stderr', resp)
                err_name, err_arg = expected
                if not (err_name in resp['stderr'] or err_name in resp['stdout']):
                    self.fail('Given exception name is not found in the output.')
                if err_arg and not (err_arg in resp['stderr'] or err_arg in resp['stdout']):
                    self.fail('Given exception argument is not found in the output.')
        if inner_exception:
            self.fail(inner_exception)

    def test_user_input(self):
        inner_exception = None
        user_input_gen = getattr(self, 'user_input', None)
        if user_input_gen is None:
            self.skipTest('No user input test cases provided for this image.')
        for idx, (code, user_input, expected) in enumerate(user_input_gen()):
            with self.subTest(subclass=idx + 1):
                try:
                    resp = self.execute(code, user_input)
                except TimeoutError as e:
                    inner_exception = AssertionError('Timeout detected at sub-case {}'.format(idx + 1))
                    break
                self.assertIn('stdout', resp)
                self.assertIn(expected, resp['stdout'])
        if inner_exception:
            self.fail(inner_exception)


class Python2ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-python2'

    def basic_success(self):
        yield 'print "hello world"', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint c', '3'

    def basic_failure(self):
        yield '!@*@*@*!', ('SyntaxError', None)
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)

    def user_input(self):
        yield "name = raw_input('>> ')\nprint('Hello, %s' % name)", 'ASDF', 'Hello, ASDF'


_simple_plot_example = '''
import numpy as np
import matplotlib.pyplot as plt

N = 20
theta = np.linspace(0.0, 2 * np.pi, N, endpoint=False)
radii = 10 * np.random.rand(N)
width = np.pi / 4 * np.random.rand(N)

ax = plt.subplot(111, projection='polar')
bars = ax.bar(theta, radii, width=width, bottom=0.0)

for r, bar in zip(radii, bars):
    bar.set_facecolor(plt.cm.jet(r / 10.))
    bar.set_alpha(0.5)

plt.show()
plt.close()

print(N, width)
'''


class Python3ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-python3'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'
        yield 'import numpy as np; import matplotlib\nprint("ok")', 'ok'
        yield _simple_plot_example, '20'

    def basic_failure(self):
        yield '!@*@*@*!', ('SyntaxError', None)
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)

    def user_input(self):
        yield "name = input('>> ')\nprint(f'Hello, {name}')", 'ASDF', 'Hello, ASDF'


_simple_tf_example = '''
import tensorflow as tf
a = tf.constant(10)
b = tf.constant(20)
c = a + b
d = tf.constant('hello')
sess = tf.Session()
print(sess.run(c))
print(sess.run(d))
sess.close()
'''


# NOTE: From the next version of TensorFlow (0.12),
#       initialize_all_variables() should be changed to global_variables_initializer().
#       The former one is deprecated and will be removed after March 2017.

_complex_tf_example = '''
import tensorflow as tf
import numpy as np
x_data = np.random.rand(100).astype(np.float32)
y_data = x_data * 0.1 + 0.3
with tf.device('/cpu:0'):
    W = tf.Variable(tf.random_uniform([1], -1.0, 1.0))
    b = tf.Variable(tf.zeros([1]))
    y = W * x_data + b
    loss = tf.reduce_mean(tf.square(y - y_data))
    optimizer = tf.train.GradientDescentOptimizer(0.5)
    train = optimizer.minimize(loss)
    sess = tf.Session(config=tf.ConfigProto(log_device_placement=True))
    sess.run(tf.global_variables_initializer())
    for step in range(201):
        sess.run(train)
        if step % 20 == 0:
            print(step, sess.run(W), sess.run(b))
    sess.close()
print('done')
'''

_gpu_detect_example = '''
import ctypes, ctypes.util
_libcuda_path = ctypes.util.find_library('cudart')
print(_libcuda_path)
_libcuda = ctypes.CDLL(_libcuda_path)
count = ctypes.c_int(0)
ret = _libcuda.cudaGetDeviceCount(ctypes.byref(count))
print('Number of GPUs detected: {0} / ret = {1}'.format(count.value, ret))
'''

_complex_tf_gpu_example = '''
import tensorflow as tf
import numpy as np
x_data = np.random.rand(100).astype(np.float32)
y_data = x_data * 0.1 + 0.3
with tf.device('/gpu:0'):
    W = tf.Variable(tf.random_uniform([1], -1.0, 1.0))
    b = tf.Variable(tf.zeros([1]))
    y = W * x_data + b
    loss = tf.reduce_mean(tf.square(y - y_data))
    optimizer = tf.train.GradientDescentOptimizer(0.5)
    train = optimizer.minimize(loss)
    sess = tf.Session(config=tf.ConfigProto(log_device_placement=True))
    sess.run(tf.global_variables_initializer())
    for step in range(201):
        sess.run(train)
        if step % 20 == 0:
            print(step, sess.run(W), sess.run(b))
    sess.close()
print('done')
'''

_tf_tutorial_mnist_load = '''
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("./samples/MNIST_data/", one_hot=True)
print('done')
'''

_keras_tf_example = '''
import numpy as np
from keras.models import Sequential
from keras.layers import Dense
input_dim = 16
num_hidden = 8
num_class = 4
batch_size = 32
model = Sequential()
model.add(Dense(num_hidden, input_dim=input_dim))
model.add(Dense(num_class))
model.compile(loss='mse', optimizer='sgd')
x = np.random.random((batch_size, input_dim))
y = np.random.random((batch_size, num_class))
model.fit(x, y)
model.pop()
print(len(model.layers))
print(model.output_shape)
'''


class Python3TensorFlowImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-python3-tensorflow'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'
        yield _simple_tf_example, '30'
        yield _complex_tf_example, 'done'
        # Keras prints to stderr which backend is active upon backend initialization.
        # https://github.com/raindeer/keras/commit/60bf55badb36273c9a65da6242699655f73b122f
        yield 'import keras; print(keras.__name__)', 'keras', 'Using TensorFlow backend.'
        yield _keras_tf_example, '1\n(None, 8)'

    def basic_failure(self):
        yield '!@*@*@*!', ('SyntaxError', None)
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)

    def user_input(self):
        yield "name = input('>> ')\nprint(f'Hello, {name}')", 'ASDF', 'Hello, ASDF'


class Python3TensorFlowGPUImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-python3-tensorflow-gpu'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'
        yield _gpu_detect_example, 'ret = 0'
        yield _simple_tf_example, '30'
        yield _complex_tf_gpu_example, 'done'
        yield _tf_tutorial_mnist_load, 'done'
        # Keras prints to stderr which backend is active upon backend initialization.
        # https://github.com/raindeer/keras/commit/60bf55badb36273c9a65da6242699655f73b122f
        yield 'import keras; print(keras.__name__)', 'keras', 'Using TensorFlow backend.'
        yield _keras_tf_example, '1\n(None, 8)'
        # If you encounter long delay on this test, try repeating the last sub-case.
        # If further runs takes short time, you need to rebuild tensorflow to match
        # your GPU's CUDA compute capabilities to avoid JIT-ing ptx codes on the first run.

    def basic_failure(self):
        yield '!@*@*@*!', ('SyntaxError', None)
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)

    def user_input(self):
        yield "name = input('>> ')\nprint(f'Hello, {name}')", 'ASDF', 'Hello, ASDF'


_py3_caffe_example = '''
import numpy as np
import caffe
caffe.set_mode_cpu()
print('done')
'''

class Python3CaffeImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-python3-caffe'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'
        yield _py3_caffe_example, 'done'

    def basic_failure(self):
        yield '!@*@*@*!', ('SyntaxError', None)
        yield 'raise RuntimeError("asdf")', ('RuntimeError', 'asdf')
        yield 'x = 0 / 0', ('ZeroDivisionError', None)

    def user_input(self):
        yield "name = input('>> ')\nprint(f'Hello, {name}')", 'ASDF', 'Hello, ASDF'


class R3ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-r3'

    def basic_success(self):
        yield 'cat("hello world\n")', 'hello world'
        yield 'a = 1;b = 2;c = a + b;cat(c, "\n")', '3'
        yield 'library("ggplot2"); cat("success\n")', 'success'

    def basic_failure(self):
        yield 'stop("my error")', ('my error', None)
        yield 'some_undefined_func()', ('could not find function', None)
        # checks if environment is properly isolated.
        yield 'print(ctx)', ("object 'ctx' not found", None)


class MROImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-r:mro-3.4.2-ubuntu'

    def basic_success(self):
        yield 'cat("hello world\n")', 'hello world'
        yield 'a = 1;b = 2;c = a + b;cat(c, "\n")', '3'
        # yield 'library("ggplot2"); cat("success\n")', 'success'

    def basic_failure(self):
        yield 'stop("my error")', ('my error', None)
        yield 'some_undefined_func()', ('could not find function', None)
        # checks if environment is properly isolated.
        yield 'print(ctx)', ("object 'ctx' not found", None)


class PHP7ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-php7'

    def basic_success(self):
        yield 'echo "hello world";', 'hello world'
        yield '$a = 1; $b = 2; $c = $a + $b; echo "$c";', '3'
        yield 'echo isset($my_nonexistent_variable) ? "1" : "0";', '0'
        # checks if our internal REPL code is NOT exposed.
        yield 'echo isset($context) ? "1" : "0";', '0'
        yield 'global $context; echo isset($context) ? "1" : "0";', '0'

    def basic_failure(self):
        yield 'throw new Exception("asdf");', ('Exception', 'asdf')
        # changed in PHP 7: divisino by zero error only occurs with modulo operator.
        # Division operator produces NAN.
        yield '$x = 0 % 0;', ('DivisionByZeroError', None)


class Nodejs6ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-nodejs6'

    def basic_success(self):
        yield 'console.log("hello world");', 'hello world'
        yield 'var a = 1; var b = 2; var c = a + b; console.log(c);', '3'
        yield 'console.log(process.version);', 'v6.'
        yield 'setTimeout(() => { console.log("async"); }, 100);', 'async'

    def basic_failure(self):
        yield 'console.log(some_undef_var);', ('ReferenceError', None)


class GitImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-git'

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


class JuliaImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-julia'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1; b = 2; c = a + b; print("sum", c)', 'sum3'
        yield 'for i in 1:5; print(i, ", "); end', '1, 2, 3, 4, 5,'
        yield 'type Person; name::AbstractString; end; print(Person("Julia"))', 'Person("Julia")'

    def basic_failure(self):
        yield 'print(some_undef_var)', ('UndefVarError', None)
        yield 'throw(ParseError("asdf"))', ('ParseError("asdf")', None)


class Lua5ImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-lua5'

    def basic_success(self):
        yield 'print("hello world")', 'hello world\n'
        yield 'io.write("hello world")', 'hello world'
        yield 'a = 1; b = 2; c = a + b; print(c)', '3'

    def basic_failure(self):
        yield 'print(some_undef_var)', ('nil', None)
        yield 'error("test-error")', ('test-error', None)


class OctaveImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-octave:latest'

    def basic_success(self):
        yield 'printf("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nc', '3'

    def basic_failure(self):
        yield 'printf(some_undef_var)', ('undefined near line 1', None)


_haskell_if_test = """
main = do
    if 7 `mod` 2 == 0
        then putStrLn "7 is even"
        else putStrLn "7 is odd"

    if 8 `mod` 4 == 0
        then putStrLn "8 is divisible by 4"
        else return ()

    let num = 9
    putStrLn $
        if num < 0
            then show num ++ " is negative"
            else if num < 10
                then show num ++ " has 1 digit"
                else show num ++ " has multiple digits"
"""

_haskell_for_test = """
import Control.Monad.Cont

main = do
    forM_ [1..3] $ \i -> do
        print i

    forM_ [7..9] $ \j -> do
        print j

    withBreak $ \\break ->
        forM_ [1..] $ \_ -> do
            p "loop"
            break ()

    where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn
"""

_haskell_function_test = """
plus :: Int -> Int -> Int
plus = (+)

plusPlus :: Int -> Int -> Int -> Int
plusPlus a b c = a + b + c

main = do
    let res = plus 1 2
    putStrLn $ "1+2 = " ++ show res

    let res = plusPlus 1 2 3
    putStrLn $ "1+2+3 = " ++ show res
"""

class HaskellImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-haskell'

    def basic_success(self):
        yield 'main :: IO ()\nmain = putStrLn "hello world!"', 'hello world!\n'
        yield _haskell_function_test, '1+2 = 3\n1+2+3 = 6\n'
        yield _haskell_if_test, '7 is odd\n8 is divisible by 4\n9 has 1 digit\n'
        yield _haskell_for_test, '1\n2\n3\n7\n8\n9\nloop'

    # Exception handling is not yet supported
    # def basic_failure(self):
    #     yield '1', ('ParseError', None)


_c_if_test = """
#include <stdio.h>
int main() 
{
    int a = 1;
    if (1 == a) { printf("true"); }
    else { printf("false"); }
}
"""

_c_for_test = """
#include <stdio.h>
int main() 
{
    int i;
    for (i = 0; i < 3; i++) { printf("%d ", i*2); }
}
"""

class CImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-c'

    def basic_success(self):
        yield '#include <stdio.h>\nint main() { printf("hello\\n"); }', 'hello\n'
        yield '#include <stdio.h>\nint main() { printf("%d",1+2); }', '3'
        yield _c_if_test, 'true'
        yield _c_for_test, '0 2 4'


_cpp_if_test = """
#include <iostream>
using namespace std;
int main() 
{
    int a = 1;
    if (1 == a) { cout << "true"; }
    else { cout << "false"; }
}
"""

_cpp_for_test = """
#include <iostream>
using namespace std;
int main() 
{
    for (int i = 0; i < 3; i++) { cout << i*2; }
}
"""

class CPPImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-cpp'

    def basic_success(self):
        yield '#include <iostream>\nusing namespace std;\nint main() { cout << "hello"; }', 'hello'
        yield '#include <iostream>\nusing namespace std;\nint main() { cout << 1+2; }', '3'
        yield _cpp_if_test, 'true'
        yield _cpp_for_test, '024'


_go_hello_test = """
package main
import "fmt"
func main() {
    fmt.Println("hello")
}
"""

_go_var_test = """
package main
import "fmt"
func main() {
    var b, c int = 1, 2
    fmt.Println(b, c)
    var d = true
    fmt.Println(d)
}
"""

_go_if_test = """
package main
import "fmt"
func main() {
    if 7%2 == 0 {
        fmt.Println("7 is even")
    } else {
        fmt.Println("7 is odd")
    }
}
"""

_go_for_test = """
package main
import "fmt"
func main() {
    i := 1
    for i <= 3 {
        fmt.Println(i)
        i = i + 1
    }
}
"""

class GoImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-go'

    def basic_success(self):
        yield _go_hello_test, 'hello'
        yield _go_var_test, '1 2\ntrue'
        yield _go_if_test, '7 is odd'
        yield _go_for_test, '1\n2\n3'


_java_hello_test = """
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World");
    }
}
"""

_java_hello_test2 = """
class Dummy {}

public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World");
    }
}
"""

_java_hello_test3 = """
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World");
    }
    public class Dummy {}
}
"""

_java_reverse_number_test = """
public class ReverseNumberWhile
{
   public static void main(String args[])
   {
      int num = 1234;
      int reversenum = 0;
      //While Loop: Logic to find out the reverse number
      while( num != 0 )
      {
          reversenum = reversenum * 10;
          reversenum = reversenum + num%10;
          num = num/10;
      }
      System.out.println("Reverse of input number is: "+ reversenum);
   }
}
"""

class JavaImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-java'

    def basic_success(self):
        yield _java_hello_test, 'Hello, World'
        yield _java_hello_test2, 'Hello, World'
        yield _java_hello_test3, 'Hello, World'
        yield _java_reverse_number_test, '4321'


_rust_if_test = """
fn main() {
    let n = 5;

    if n < 0 {
        print!("{} is negative", n);
    } else if n > 0 {
        print!("{} is positive", n);
    } else {
        print!("{} is zero", n);
    }
}
"""

_rust_loop_test = """
fn main() {
    let mut count = 0u32;
    loop {
        count += 1;
        println!("{}", count);
        if count == 3 {
            break;
        }
    }
}
"""

class RustImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-rust'

    def basic_success(self):
        yield 'fn main() { println!("Hello, World!"); }', 'Hello, World'
        yield _rust_if_test, '5 is positive'
        yield _rust_loop_test, '1\n2\n3'


_cntk_numpy_test = """
import cntk
import numpy as np
x = cntk.input_variable(2)
y = cntk.input_variable(2)
x0 = np.asarray([[2., 1.]], dtype=np.float32)
y0 = np.asarray([[4., 6.]], dtype=np.float32)
print(cntk.squared_error(x, y).eval({x:x0, y:y0}))
"""


class CNTKImageTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/python-cntk:2.2-py36'

    def basic_success(self):
        yield 'print("hello world")', "hello world"
        yield 'import cntk; print(cntk.__version__)', '2.2'
        yield 'import cntk; print(cntk.minus([1, 2, 3], [4, 5, 6]).eval())', \
              '[-3. -3. -3.]'
        yield _cntk_numpy_test, '[ 29.]'
        yield 'import keras; print(keras.__name__)', 'keras', 'Using CNTK backend'


class JailTest(ImageTestBase, unittest.TestCase):

    image_name = 'lablup/kernel-python3'

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
