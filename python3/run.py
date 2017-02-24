#! /usr/bin/env python

import builtins as builtin_mod
import code
import enum
from functools import partial
import logging
from namedlist import namedtuple, namedlist
import os
import sys
import time
import traceback
import types

import simplejson as json
import zmq

import getpass

from sorna.types import (
    InputRequest, ControlRecord, ConsoleRecord, MediaRecord, HTMLRecord,
)

log = logging.getLogger('code-runner')


class StreamToEmitter:

    def __init__(self, emitter, stream_type):
        self.emit = emitter
        self.stream_type = stream_type

    def write(self, s):
        self.emit(ConsoleRecord(self.stream_type, s))

    def flush(self):
        pass


class CodeRunner:
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self, api_version=1):
        self.api_version = api_version
        self.input_supported = (api_version >= 2)

        ctx = zmq.Context.instance()
        self.input_stream = ctx.socket(zmq.PULL)
        self.input_stream.bind('tcp://*:2000')
        self.output_stream = ctx.socket(zmq.PUSH)
        self.output_stream.bind('tcp://*:2001')

        self.stdout_emitter = StreamToEmitter(self.emit, 'stdout')
        self.stderr_emitter = StreamToEmitter(self.emit, 'stderr')

        # Initialize user module and namespaces.
        user_module = types.ModuleType('__main__',
                                       doc='Automatically created module for the interactive shell.')
        user_module.__dict__.setdefault('__builtin__', builtin_mod)
        user_module.__dict__.setdefault('__builtins__', builtin_mod)
        self.user_module = user_module
        self.user_ns = user_module.__dict__

    def handle_input(self, prompt=None, password=False):
        if prompt is None:
            prompt = 'Password: ' if password else ''
        self.emit(ConsoleRecord('stdout', prompt))
        self.emit(InputRequest(is_password=password))
        data = self.input_stream.recv_multipart()
        return data[1].decode('utf8')

    def emit(self, record):
        if isinstance(record, ConsoleRecord):
            assert record.target in ('stdout', 'stderr')
            self.output_stream.send_multipart([
                record.target.encode('ascii'),
                record.data.encode('utf8'),
            ])
        elif isinstance(record, MediaRecord):
            self.output_stream.send_multipart([
                b'media',
                json.dumps({
                    'type': record.type,
                    'data': record.data,
                }).encode('utf8'),
            ])
        elif isinstance(record, HTMLRecord):
            self.output_stream.send_multipart([
                b'html',
                record.html.encode('utf8'),
            ])
        elif isinstance(record, InputRequest):
            self.output_stream.send_multipart([
                b'waiting-input',
                json.dumps({
                    'is_password': record.is_password,
                }).encode('utf8'),
            ])
        elif isinstance(record, ControlRecord):
            self.output_stream.send_multipart([
                record.event.encode('ascii'),
                b'',
            ])
        else:
            raise TypeError('Unsupported record type.')

    @staticmethod
    def strip_traceback(tb):
        while tb is not None:
            frame_summary = traceback.extract_tb(tb, limit=1)[0]
            if frame_summary[0] == '<input>':
                break
            tb = tb.tb_next
        return tb

    def run(self):
        json_opts = {'namedtuple_as_object': False}
        while True:
            data = self.input_stream.recv_multipart()
            code_id = data[0].decode('ascii')
            code_text = data[1].decode('utf8')
            self.user_module.__builtins__._sorna_emit = self.emit
            if self.input_supported:
                self.user_module.__builtins__.input = self.handle_input
                getpass.getpass = partial(self.handle_input, password=True)
            try:
                code_obj = code.compile_command(code_text, symbol='exec')
            except (OverflowError, IndentationError, SyntaxError,
                    ValueError, TypeError, MemoryError) as e:
                exc_type, exc_val, tb = sys.exc_info()
                user_tb = type(self).strip_traceback(tb)
                err_str = ''.join(traceback.format_exception(exc_type, exc_val, user_tb))
                hdr_str = 'Traceback (most recent call last):\n' if not err_str.startswith('Traceback ') else ''
                self.emit(ConsoleRecord('stderr', hdr_str + err_str))
                self.emit(ControlRecord('finished'))
            else:
                sys.stdout, orig_stdout = self.stdout_emitter, sys.stdout
                sys.stderr, orig_stderr = self.stderr_emitter, sys.stderr
                try:
                    exec(code_obj, self.user_ns)
                except Exception as e:
                    # strip the first frame
                    exc_type, exc_val, tb = sys.exc_info()
                    user_tb = type(self).strip_traceback(tb)
                    traceback.print_exception(exc_type, exc_val, user_tb)
                finally:
                    self.emit(ControlRecord('finished'))
                    sys.stdout = orig_stdout
                    sys.stderr = orig_stderr


def main():
    log = logging.getLogger('main')

    # Replace stdin with a "null" file
    # (trying to read stdin will raise EOFError immediately afterwards.)
    sys.stdin = open(os.devnull, 'rb')

    # Initialize context object.
    runner = CodeRunner(api_version=2)
    try:
        runner.run()
    except (KeyboardInterrupt, SystemExit):
        pass
    except:
        log.exception('unexpected error')
    finally:
        print('exit.')


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    main()
