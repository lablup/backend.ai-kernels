#! /usr/bin/env python3

import builtins as builtin_mod
import code
import io
from namedlist import namedtuple, namedlist
import os
import subprocess
import sys
import types
import zmq
try:
    import simplejson
    has_simplejson = True
except ImportError:
    has_simplejson = False

import sorna.drawing

ExceptionInfo = namedtuple('ExceptionInfo', [
    'exc',
    ('args', tuple()),
    ('raised_before_exec', False),
    ('traceback', None),
])

Result = namedlist('Result', [
    ('stdout', ''),
    ('stderr', ''),
    ('media', None),
])


@staticmethod
def _create_excinfo(e, raised_before_exec, tb):
    assert isinstance(e, Exception)
    return ExceptionInfo(type(e).__name__, e.args, raised_before_exec, tb)
ExceptionInfo.create = _create_excinfo


class CodeRunner(object):
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self):
        self.stdout_buffer = io.StringIO()
        self.stderr_buffer = io.StringIO()

        # Initialize user module and namespaces.
        user_module = types.ModuleType('__main__',
                                       doc='Automatically created module for the interactive shell.')
        user_module.__dict__.setdefault('__builtin__', builtin_mod)
        user_module.__dict__.setdefault('__builtins__', builtin_mod)
        self.user_module = user_module
        self.user_ns = user_module.__dict__

    def execute(self, cell_id, src):
        sys.stdout, orig_stdout = self.stdout_buffer, sys.stdout
        sys.stderr, orig_stderr = self.stderr_buffer, sys.stderr

        exceptions = []
        result = Result()

        def my_excepthook(type_, value, tb):
            exceptions.append(ExceptionInfo.create(value, False, tb))
        sys.excepthook = my_excepthook

        self.user_module.__builtins__._sorna_media = []
        try:
            if src.startswith('%'):
                # TODO: do special commands.
                raise NotImplementedError()
            else:
                # execute shell commands.
                completed = subprocess.run(src, shell=True, check=False,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.PIPE)
                self.stdout_buffer.write(completed.stdout.decode())
                self.stderr_buffer.write(completed.stderr.decode())
        except Exception as e:
            exceptions.append(ExceptionInfo.create(e, False, None))

        sys.excepthook = sys.__excepthook__

        result.stdout = self.stdout_buffer.getvalue()
        result.stderr = self.stderr_buffer.getvalue()
        result.media = self.user_module.__builtins__._sorna_media
        self.stdout_buffer.seek(0, io.SEEK_SET)
        self.stdout_buffer.truncate(0)
        self.stderr_buffer.seek(0, io.SEEK_SET)
        self.stderr_buffer.truncate(0)

        sys.stdout = orig_stdout
        sys.stderr = orig_stderr
        return exceptions, result


if __name__ == '__main__':
    # Use the "confined" working directory
    os.chdir('/home/work')
    # Replace stdin with a "null" file
    # (trying to read stdin will raise EOFError immediately afterwards.)
    sys.stdin = open(os.devnull, 'rb')

    # Initialize context object.
    runner = CodeRunner()

    # Initialize minimal ZMQ server socket.
    ctx = zmq.Context(io_threads=1)
    sock = ctx.socket(zmq.REP)
    sock.bind('tcp://*:2001')
    print('serving at port 2001...')

    try:
        while True:
            data = sock.recv_multipart()
            exceptions, result = runner.execute(data[0].decode('ascii'),
                                                data[1].decode('utf8'))
            response = {
                'stdout': result.stdout,
                'stderr': result.stderr,
                'media': result.media,
                'exceptions': exceptions,
            }
            json_opts = {}
            if has_simplejson:
                json_opts['namedtuple_as_object'] = False
            sock.send_json(response, **json_opts)
    except (KeyboardInterrupt, SystemExit):
        pass
    finally:
        sock.close()
        print('exit.')
