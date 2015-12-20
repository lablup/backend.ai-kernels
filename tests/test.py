#! /usr/bin/env python3
from abc import abstractmethod
import unittest


class ImageTestBase(object):

    image_id = ''

    @abstractmethod
    def basic_success(self):
        yield None, None

    def setUp(self):
        print('Creating container from {}'.format(self.image_id))
        # TODO: implement

    def tearDown(self):
        print('Destroying the container')
        # TODO: implement

    def test_basic_success(self):
        print('Image ID: {}'.format(self.image_id))
        for idx, (code, output) in enumerate(self.basic_success()):
            with self.subTest(idx):
                print('[Test {}]: input: {!r}, expected output: {!r}'.format(idx, code, output))
                # TODO: implement


class Python34ImageTest(ImageTestBase, unittest.TestCase):

    image_id = 'kernel-python34'

    def basic_success(self):
        yield 'print("hello world")', 'hello world'
        yield 'a = 1\nb = 2\nc = a + b\nprint(c)', '3'


class PHP55ImageTest(ImageTestBase, unittest.TestCase):

    image_id = 'kernel-php55'

    def basic_success(self):
        yield 'echo "hello world";', 'hello world'
        yield '$a = 1; $b = 2; $c = $a + $b; echo "$c";', '3'

class Node34ImageTest(ImageTestBase, unittest.TestCase):

    image_id = 'kernel-node34'

    def basic_success(self):
        yield 'console.log("hello world");', 'hellow world'
        yield 'var a = 1; var b = 2; var c = a + b; console.log(c);', '3'


if __name__ == '__main__':
    unittest.main()
