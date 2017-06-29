#! /usr/bin/env python3

import logging
import os
from pathlib import Path
import shlex
import sys
import tempfile

sys.path.insert(0, os.path.abspath('.'))
from base_run import BaseRun
# For debugging
# sys.path.insert(0, os.path.abspath('..'))
# from base.run import BaseRun

log = logging.getLogger()

DEFAULT_CFLAGS = '-Wall'
DEFAULT_LDFLAGS = '-lrt -lm -pthread'
CHILD_ENV = {
    'TERM': 'xterm',
    'LANG': 'C.UTF-8',
    'SHELL': '/bin/ash',
    'USER': 'work',
    'HOME': '/home/work',
    'PATH': '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
}


class Run(BaseRun):

    def __init__(self):
        super().__init__()
        self.child_env = CHILD_ENV

    async def build(self, build_cmd):
        if build_cmd is None or build_cmd == '':
            # skipped
            return
        elif build_cmd == '*':
            # use the default heuristic
            if Path('Makefile').is_file():
                await self.run_subproc('make')
            elif Path('main.c').is_file():
                cfiles = Path('.').glob('**/*.c')
                cfiles = ' '.join(map(lambda p: shlex.quote(str(p)), cfiles))
                cmd = (f'gcc {cfiles} {DEFAULT_CFLAGS} -o ./main {DEFAULT_LDFLAGS}; '
                       f'chmod 755 ./main')
                await self.run_subproc(cmd)
            else:
                self.outsock.write([
                    b'stderr',
                    b'c-kernel: cannot find build script ("Makefile").\n',
                ])
        else:
            await self.run_subproc(build_cmd)

    async def execute(self, exec_cmd):
        if exec_cmd is None or exec_cmd == '':
            # skipped
            return
        elif exec_cmd == '*':
            if Path('./main').is_file():
                await self.run_subproc('chmod 755 ./main; ./main')
            elif Path('./a.out').is_file():
                await self.run_subproc('chmod 755 ./a.out; ./a.out')
            else:
                self.outsock.write([
                    b'stderr',
                    b'c-kernel: cannot find executable ("a.out" or "main").\n',
                ])
        else:
            await self.run_subproc(exec_cmd)

    async def query(self, code_text):
        with tempfile.NamedTemporaryFile(suffix='.c', dir='.') as tmpf:
            tmpf.write(code_text)
            tmpf.flush()
            cmd = (f'gcc {tmpf.name} {DEFAULT_CFLAGS} -o ./main {DEFAULT_LDFLAGS} '
                   f'&& chmod 755 ./main && ./main')
            await self.run_subproc(cmd)


def main():
    Run().run()

if __name__ == '__main__':
    main()
