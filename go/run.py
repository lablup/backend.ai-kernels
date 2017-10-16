#! /usr/bin/env python3
import logging
import os
from pathlib import Path
import shlex
import sys
import tempfile

sys.path.insert(0, os.path.abspath('.'))
from base_run import BaseRunner
# For debugging
# sys.path.insert(0, os.path.abspath('..'))
# from base.run import BaseRun

log = logging.getLogger()

DEFAULT_BFLAGS = ''
CHILD_ENV = {
    'TERM': 'xterm',
    'LANG': 'C.UTF-8',
    'SHELL': '/bin/ash',
    'USER': 'work',
    'HOME': '/home/work',
    'PATH': '/go/bin:/usr/local/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
    'GOPATH': '/home/work',
}


class GoProgramRunner(BaseRunner):

    log_prefix = 'go-kernel'

    def __init__(self):
        super().__init__()
        self.child_env.update(CHILD_ENV)

    async def init_with_loop(self):
        pass

    async def build(self, build_cmd):
        if build_cmd is None or build_cmd == '':
            # skipped
            return
        elif build_cmd == '*':
            # use the default heuristic
            if Path('main.go').is_file():
                gofiles = Path('.').glob('**/*.go')
                gofiles = ' '.join(map(lambda p: shlex.quote(str(p)), gofiles))
                cmd = f'go build -o main {DEFAULT_BFLAGS} {gofiles}'
                await self.run_subproc(cmd)
            else:
                log.error('cannot find main file ("main.go").')
        else:
            await self.run_subproc(build_cmd)

    async def execute(self, exec_cmd):
        if exec_cmd is None or exec_cmd == '':
            # skipped
            return
        elif exec_cmd == '*':
            if Path('./main').is_file():
                await self.run_subproc('./main')
            else:
                log.error('cannot find executable ("main").')
        else:
            await self.run_subproc(exec_cmd)

    async def query(self, code_text):
        with tempfile.NamedTemporaryFile(suffix='.go', dir='.') as tmpf:
            tmpf.write(code_text.encode('utf8'))
            tmpf.flush()
            cmd = f'go run {tmpf.name}'
            await self.run_subproc(cmd)

    async def complete(self, data):
        return []

    async def interrupt(self):
        # subproc interrupt is already handled by BaseRunner
        pass


if __name__ == '__main__':
    GoProgramRunner().run()
