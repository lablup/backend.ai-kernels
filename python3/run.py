#! /usr/bin/env python

import logging
import os
from pathlib import Path
import sys

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
        if self.inproc_runner is None:
            self.inproc_runner = PythonInprocRunner(self)
        # NOTE: In-process code execution is a blocking operation.
        self.inproc_runner.query(code_text)

    async def complete(self, data):
        try:
            return self.inproc_runner.get_completions(data)
        except:
            log.exception('unexpected error')


if __name__ == '__main__':
    PythonProgramRunner().run()
