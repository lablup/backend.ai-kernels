#! /usr/bin/env python

import builtins as builtin_mod
import code
from concurrent.futures import ThreadPoolExecutor, TimeoutError
import enum
import io
import logging
from namedlist import namedtuple, namedlist, FACTORY
import os
from os import path
import queue
import sys
import threading
import time
import traceback
import types

import simplejson as json
import zmq

ExceptionInfo = namedtuple('ExceptionInfo', [
    'exc',
    ('args', tuple()),
    ('raised_before_exec', False),
    ('traceback', None),
])

# v1 API

ResultV1 = namedlist('ResultV1', [
    ('stdout', ''),
    ('stderr', ''),
    ('media', None),
])

# v2 API

InputRequest = namedtuple('InputRequest', [
    ('awaiting', False),
    ('is_password', False),
])

ConsoleRecord = namedtuple('ConsoleRecord', [
    ('target', 'stdout'),  # or 'stderr'
    ('data', ''),
])

MediaRecord = namedtuple('MediaRecord', [
    ('type', None),  # mime-type
    ('data', None),
])

ResultV2 = namedlist('ResultV2', [
    ('input', FACTORY(InputRequest)),
    ('output', FACTORY(list)),  # list of ConsoleRecord
    ('media', FACTORY(list)),   # list of MediaRecord
])

log = logging.getLogger('code-runner')


@staticmethod
def _create_excinfo(e, raised_before_exec=False, tb=None):
    assert isinstance(e, Exception)
    return ExceptionInfo(type(e).__name__, e.args, raised_before_exec, tb)
ExceptionInfo.create = _create_excinfo


class ContinuationStatus(enum.Enum):
    FINISHED = 'finished'
    CONTINUED = 'continued'
    WAITING_INPUT = 'waiting-input'


class InputRequestPending(Exception):
    pass


class UserCodeFinished(Exception):
    pass


class CodeRunner(object):
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self, api_version=1, input_supported=True):
        self.api_version = api_version
        self.input_supported = input_supported

        self.stdout_buffer = io.StringIO()
        self.stderr_buffer = io.StringIO()

        self.input_event = threading.Event()
        self.input_queue = queue.Queue()
        self.executor = ThreadPoolExecutor(max_workers=1)

        # Initialize user module and namespaces.
        user_module = types.ModuleType('__main__',
                                       doc='Automatically created module for the interactive shell.')
        user_module.__dict__.setdefault('__builtin__', builtin_mod)
        user_module.__dict__.setdefault('__builtins__', builtin_mod)
        self.user_module = user_module
        self.user_ns = user_module.__dict__

    def handle_input(self, prompt=None, password=False):
        print(prompt, end='')
        self.input_event.set()
        return self.input_queue.get()

    def flush_console(self):
        self.result.stdout = self.stdout_buffer.getvalue()
        self.result.stderr = self.stderr_buffer.getvalue()
        self.result.media = self.user_module.__builtins__._sorna_media
        self.user_module.__builtins__._sorna_media = []
        self.stdout_buffer.seek(0, io.SEEK_SET)
        self.stdout_buffer.truncate(0)
        self.stderr_buffer.seek(0, io.SEEK_SET)
        self.stderr_buffer.truncate(0)

    @staticmethod
    def strip_traceback(tb):
        while tb is not None:
            frame_summary = traceback.extract_tb(tb, limit=1)[0]
            if frame_summary[0] == '<input>':
                break
            tb = tb.tb_next
        return tb

    def execute(self, code_id, code_text):
        self.exceptions = []

        self.user_module.__builtins__._sorna_media = []
        if self.input_supported:
            self.user_module.__builtins__.input = self.handle_input

        self.user_module.__builtins__._sorna_media = []
        self.result = ResultV1()
        try:
            code_obj = code.compile_command(code_text, symbol='exec')
        except (OverflowError, IndentationError, SyntaxError,
                ValueError, TypeError, MemoryError) as e:
            #self.exceptions.append(ExceptionInfo.create(e, True, None))
            exc_type, exc_val, tb = sys.exc_info()
            user_tb = type(self).strip_traceback(tb)
            err_str = ''.join(traceback.format_exception(exc_type, exc_val, user_tb))
            hdr_str = 'Traceback (most recent call last):\n' if not err_str.startswith('Traceback ') else ''
            self.result.stderr = hdr_str + err_str
            self.has_early_exception = True
        else:

            def run_user_code():
                sys.stdout, orig_stdout = self.stdout_buffer, sys.stdout
                sys.stderr, orig_stderr = self.stderr_buffer, sys.stderr
                try:
                    exec(code_obj, self.user_ns)
                except Exception as e:
                    # strip the first frame
                    exc_type, exc_val, tb = sys.exc_info()
                    user_tb = type(self).strip_traceback(tb)
                    traceback.print_exception(exc_type, exc_val, user_tb)
                finally:
                    sys.stdout = orig_stdout
                    sys.stderr = orig_stderr

            self.has_early_exception = False
            self.future = self.executor.submit(run_user_code)

        return self

    def __enter__(self):
        return self

    def __iter__(self):

        def _continuation():
            if self.has_early_exception:
                yield ContinuationStatus.FINISHED, self.result
                return

            self.exceptions = []
            self.result = ResultV1()
            try:
                while not self.future.done():
                    try:
                        for _ in range(10):
                            if self.input_event.wait(0.2):
                                self.input_event.clear()
                                raise InputRequestPending
                            if self.future.done():
                                raise UserCodeFinished
                        else:
                            raise TimeoutError
                    except InputRequestPending:
                        self.flush_console()
                        code_id, user_input = yield ContinuationStatus.WAITING_INPUT, self.result
                        self.exceptions = []
                        self.result = ResultV1()
                        self.input_queue.put(user_input)
                    except UserCodeFinished:
                        break
                    except TimeoutError:
                        self.flush_console()
                        yield ContinuationStatus.CONTINUED, self.result
                        self.exceptions = []
                        self.result = ResultV1()
            except:
                log.exception('unexpected error')
            finally:
                self.flush_console()
                yield ContinuationStatus.FINISHED, self.result

        return _continuation()

    def __exit__(self, exc_type, exc_value, tb):
        pass


def main():
    log = logging.getLogger('main')

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

    json_opts = {'namedtuple_as_object': False}

    try:
        while True:
            data = sock.recv_multipart()
            code_id = data[0].decode('ascii')
            code_text = data[1].decode('utf8')
            with runner.execute(code_id, code_text) as continuation:
                try:
                    gen = iter(continuation)
                    status, result = next(gen)
                    while True:
                        sock.send_json({
                            'stdout': result.stdout,
                            'stderr': result.stderr,
                            'media': result.media,
                            'exceptions': [],
                            'status': status.value,
                        }, **json_opts)
                        if status == ContinuationStatus.FINISHED:
                            next(gen)  # run until return
                            break
                        data = sock.recv_multipart()
                        code_id = data[0].decode('ascii')
                        code_text = data[1].decode('utf8')
                        status, result = gen.send((code_id, code_text))
                except StopIteration:
                    pass
    except (KeyboardInterrupt, SystemExit):
        pass
    except:
        log.exception('unexpected error')
    finally:
        sock.close()
        print('exit.')


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    main()
