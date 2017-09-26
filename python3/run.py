#! /usr/bin/env python

import asyncio
import ctypes
import logging
import os
from pathlib import Path
import sys
import threading

import janus
import simplejson as json

sys.path.insert(0, os.path.abspath('.'))
from base_run import BaseRunner
from inproc_run import PythonInprocRunner

log = logging.getLogger()

DEFAULT_PYFLAGS = ''
CHILD_ENV = {
    'TERM': 'xterm',
    'LANG': 'C.UTF-8',
    'SHELL': '/bin/ash',
    'USER': 'work',
    'HOME': '/home/work',
    'PATH': '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
    'LD_PRELOAD': '/home/sorna/patch-libs.so',
}


class PythonProgramRunner(BaseRunner):

    log_prefix = 'python-kernel'

    def __init__(self):
        super().__init__()
        self.child_env.update(CHILD_ENV)
        self.inproc_runner = None
        self.sentinel = object()
        self.input_queue = None
        self.output_queue = None

    async def init_with_loop(self):
        self.input_queue = janus.Queue(loop=self.loop)
        self.output_queue = janus.Queue(loop=self.loop)

        # We have interactive input functionality!
        self._user_input_queue = janus.Queue(loop=self.loop)
        self.user_input_queue = self._user_input_queue.async_q

    async def build(self, build_cmd):
        if build_cmd is None or build_cmd == '':
            # skipped
            return
        elif build_cmd == '*':
            if Path('setup.py').is_file():
                cmd = f'python {DEFAULT_PYFLAGS} setup.py develop'
                await self.run_subproc(cmd)
            else:
                log.warning('skipping build phase due to missing "setup.py" file')
        else:
            await self.run_subproc(build_cmd)

    async def execute(self, exec_cmd):
        if exec_cmd is None or exec_cmd == '':
            # skipped
            return
        elif exec_cmd == '*':
            if Path('main.py').is_file():
                cmd = f'python {DEFAULT_PYFLAGS} main.py'
                await self.run_subproc(cmd)
            else:
                log.error('cannot find the main script ("main.py").')
        else:
            await self.run_subproc(exec_cmd)

    async def query(self, code_text):
        self.ensure_inproc_runner()
        await self.input_queue.async_q.put(code_text)
        # Read the generated outputs until done
        while True:
            try:
                msg = await self.output_queue.async_q.get()
            except asyncio.CancelledError:
                break
            self.output_queue.async_q.task_done()
            if msg is self.sentinel:
                break
            self.outsock.write(msg)

    async def complete(self, data):
        self.ensure_inproc_runner()
        matches = self.inproc_runner.complete(data)
        self.outsock.write([
            b'completion',
            json.dumps(matches).encode('utf8'),
        ])

    async def interrupt(self):
        if self.inproc_runner is None:
            log.error('No user code is running!')
            return
        # A dirty hack to raise an exception inside a running thread.
        target_tid = self.inproc_runner.ident
        if target_tid not in {t.ident for t in threading.enumerate()}:
            log.error('Interrupt failed due to missing thread.')
            return
        affected_count = ctypes.pythonapi.PyThreadState_SetAsyncExc(
            ctypes.c_long(target_tid),
            ctypes.py_object(KeyboardInterrupt))
        if affected_count == 0:
            log.error('Interrupt failed due to invalid thread identity.')
        elif affected_count > 1:
            ctypes.pythonapi.PyThreadState_SetAsyncExc(
                ctypes.c_long(target_tid),
                ctypes.c_long(0))
            log.error('Interrupt broke the interpreter state -- '
                      'recommended to reset the session.')

    def ensure_inproc_runner(self):
        if self.inproc_runner is None:
            self.inproc_runner = PythonInprocRunner(
                self.input_queue.sync_q,
                self.output_queue.sync_q,
                self._user_input_queue.sync_q,
                self.sentinel)
            self.inproc_runner.start()


if __name__ == '__main__':
    PythonProgramRunner().run()
