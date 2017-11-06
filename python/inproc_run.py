import builtins as builtin_mod
import code
import enum
from functools import partial
import logging
import os
import queue
import socket
import sys
import time
import traceback
import threading
import types

import simplejson as json
from IPython.core.completer import Completer

import getpass

from sorna.types import (
    InputRequest, ControlRecord, ConsoleRecord, MediaRecord, HTMLRecord,
)


log = logging.getLogger()

class StreamToEmitter:

    def __init__(self, emitter, stream_type):
        self.emit = emitter
        self.stream_type = stream_type

    def write(self, s):
        self.emit(ConsoleRecord(self.stream_type, s))

    def flush(self):
        pass


class PythonInprocRunner(threading.Thread):
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self, input_queue, output_queue, user_input_queue, sentinel):
        super().__init__(name='InprocRunner', daemon=True)

        # for interoperability with the main asyncio loop
        self.input_queue = input_queue
        self.output_queue = output_queue
        self.user_input_queue = user_input_queue
        self.sentinel = sentinel

        self.stdout_emitter = StreamToEmitter(self.emit, 'stdout')
        self.stderr_emitter = StreamToEmitter(self.emit, 'stderr')

        # Initialize user module and namespaces.
        user_module = types.ModuleType('__main__',
                                       doc='Automatically created module for the interactive shell.')
        user_module.__dict__.setdefault('__builtin__', builtin_mod)
        user_module.__dict__.setdefault('__builtins__', builtin_mod)
        self.user_module = user_module
        self.user_ns = user_module.__dict__

        self.completer = Completer(namespace=self.user_ns, global_namespace={})
        self.completer.limit_to__all__ = True

    def run(self):
        # User code is executed in a separate thread.
        while True:
            code_text = self.input_queue.get()
            self.input_queue.task_done()

            # Set Sorna Media handler
            self.user_module.__builtins__._sorna_emit = self.emit

            # Override interactive input functions
            self.user_module.__builtins__.input = self.handle_input
            getpass.getpass = partial(self.handle_input, password=True)

            try:
                code_obj = code.compile_command(code_text, symbol='exec')
            except (OverflowError, IndentationError, SyntaxError,
                    ValueError, TypeError, MemoryError):
                exc_type, exc_val, tb = sys.exc_info()
                user_tb = type(self).strip_traceback(tb)
                err_str = ''.join(traceback.format_exception(exc_type, exc_val, user_tb))
                hdr_str = 'Traceback (most recent call last):\n' if not err_str.startswith('Traceback ') else ''
                self.emit(ConsoleRecord('stderr', hdr_str + err_str))
                self.output_queue.put(self.sentinel)
            else:
                sys.stdout, orig_stdout = self.stdout_emitter, sys.stdout
                sys.stderr, orig_stderr = self.stderr_emitter, sys.stderr
                try:
                    exec(code_obj, self.user_ns)
                except KeyboardInterrupt:
                    self.emit(ConsoleRecord('stderr', 'Interrupted!'))
                except Exception:
                    # strip the first frame
                    exc_type, exc_val, tb = sys.exc_info()
                    user_tb = type(self).strip_traceback(tb)
                    traceback.print_exception(exc_type, exc_val, user_tb,
                                              file=sys.stderr)
                finally:
                    sys.stdout = orig_stdout
                    sys.stderr = orig_stderr
                    self.output_queue.put(self.sentinel)

    def handle_input(self, prompt=None, password=False):
        if prompt is None:
            prompt = 'Password: ' if password else ''
        # Use synchronous version of ZeroMQ sockets
        if prompt:
            self.output_queue.put([
                b'stdout',
                prompt.encode('utf8'),
            ])
        self.output_queue.put([
            b'waiting-input',
            json.dumps({'is_password': password}).encode('utf8'),
        ])
        data = self.user_input_queue.get()
        return data

    def complete(self, data):
        # This method is executed in the main thread.
        state = 0
        matches = []
        while True:
            ret = self.completer.complete(data['line'], state)
            if ret is None:
                break
            matches.append(ret)
            state += 1
        return matches

    def emit(self, record):
        if isinstance(record, ConsoleRecord):
            assert record.target in ('stdout', 'stderr')
            self.output_queue.put([
                record.target.encode('ascii'),
                record.data.encode('utf8'),
            ])
        elif isinstance(record, MediaRecord):
            self.output_queue.put([
                b'media',
                json.dumps({
                    'type': record.type,
                    'data': record.data,
                }).encode('utf8'),
            ])
        elif isinstance(record, HTMLRecord):
            self.output_queue.put([
                b'html',
                record.html.encode('utf8'),
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
