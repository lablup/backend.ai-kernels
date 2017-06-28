#! /usr/bin/env python3

import asyncio
import io
import logging
import os
from pathlib import Path
import shlex
import signal
import sys
import tempfile

from namedlist import namedtuple, namedlist
import simplejson as json
import uvloop
import zmq, aiozmq


log = logging.getLogger()


DEFAULT_CFLAGS = '-Wall'
DEFAULT_LDFLAGS = '-lrt -lm -pthread'


async def run_subproc(insock, outsock, cmd):
    '''
    A thin wrapper for an external command.
    '''
    loop = asyncio.get_event_loop()
    try:
        child_env = {
            'TERM': 'xterm',
            'LANG': 'C.UTF-8',
            'SHELL': '/bin/ash',
            'USER': 'work',
            'HOME': '/home/work',
            'PATH': '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
        }
        proc = await asyncio.create_subprocess_shell(
            cmd,
            env=child_env,
            stdin=None,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        pipe_tasks = [
            loop.create_task(pipe_output(proc.stdout, outsock, 'stdout')),
            loop.create_task(pipe_output(proc.stderr, outsock, 'stderr')),
        ]
        await proc.wait()
        for t in pipe_tasks:
            t.cancel()
            await t
    except:
        log.exception('unexpected error')


async def execute_build(insock, outsock, build_cmd):
    if build_cmd is None or build_cmd == '':
        # skipped
        return
    elif build_cmd == '*':
        # use the default heuristic
        outsock.write([
            b'stderr',
            b'c-kernel: running heuristic build step...\n',
        ])
        if Path('Makefile').is_file():
            await run_subproc(insock, outsock, 'make')
        elif Path('main.c').is_file():
            cfiles = Path('.').glob('**/*.c')
            cfiles = ' '.join(map(lambda p: shlex.quote(str(p)), cfiles))
            cmd = (f'gcc {cfiles} {DEFAULT_CFLAGS} -o ./main {DEFAULT_LDFLAGS}; '
                   f'chmod 755 ./main')
            await run_subproc(insock, outsock, cmd)
        else:
            outsock.write([
                b'stderr',
                b'c-kernel: cannot find build script ("Makefile").\n',
            ])
    else:
        await run_subproc(insock, outsock, build_cmd)


async def execute_execute(insock, outsock, exec_cmd):
    if exec_cmd is None or exec_cmd == '':
        # skipped
        return
    elif exec_cmd == '*':
        if Path('./main').is_file():
            await run_subproc(insock, outsock, 'chmod 755 ./main; ./main')
        elif Path('./a.out').is_file():
            await run_subproc(insock, outsock, 'chmod 755 ./a.out; ./a.out')
        else:
            outsock.write([
                b'stderr',
                b'c-kernel: cannot find executable ("a.out" or "main").\n',
            ])
    else:
        await run_subproc(insock, outsock, exec_cmd)


async def execute_query(insock, outsock, code_text):
    '''
    Run the user code by creating a temporary file and compiling it.
    '''
    with tempfile.NamedTemporaryFile(suffix='.c', dir='.') as tmpf:
        tmpf.write(code_text)
        tmpf.flush()
        cmd = (f'gcc {tmpf.name} {DEFAULT_CFLAGS} -o ./main {DEFAULT_LDFLAGS} '
               f'&& chmod 755 ./main && ./main')
        await run_subproc(insock, outsock, cmd)


async def pipe_output(stream, outsock, target):
    assert target in ('stdout', 'stderr')
    try:
        while True:
            data = await stream.read(4096)
            if not data:
                break
            outsock.write([target.encode('ascii'), data])
            await outsock.drain()
    except (aiozmq.ZmqStreamClosed, asyncio.CancelledError):
        pass
    except:
        log.exception('unexpected error')


async def main_loop():
    insock  = await aiozmq.create_zmq_stream(zmq.PULL, bind='tcp://*:2000')
    outsock = await aiozmq.create_zmq_stream(zmq.PUSH, bind='tcp://*:2001')
    print('start serving...')
    while True:
        try:
            data = await insock.read()
            op_type = data[0].decode('ascii')
            text = data[1].decode('utf8')
            if op_type == 'build':    # batch-mode step 1
                await execute_build(insock, outsock, text)
                outsock.write([b'build-finished', b''])
            elif op_type == 'exec':   # batch-mode step 2
                await execute_execute(insock, outsock, text)
                outsock.write([b'finished', b''])
            elif op_type == 'input':  # query-mode
                await execute_query(insock, outsock, text)
                outsock.write([b'finished', b''])
            await outsock.drain()
        except asyncio.CancelledError:
            break
        except:
            log.exception('unexpected error')
            break
    insock.close()
    outsock.close()


def main():
    # Replace stdin with a "null" file
    # (trying to read stdin will raise EOFError immediately afterwards.)
    sys.stdin = open(os.devnull, 'rb')

    # Initialize event loop.
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()
    stopped = asyncio.Event()

    def interrupt(loop, stopped):
        if not stopped.is_set():
            stopped.set()
            loop.stop()
        else:
            print('forced shutdown!', file=sys.stderr)
            sys.exit(1)

    loop.add_signal_handler(signal.SIGINT, interrupt, loop, stopped)
    loop.add_signal_handler(signal.SIGTERM, interrupt, loop, stopped)

    try:
        main_task = loop.create_task(main_loop())
        loop.run_forever()
        # interrupted
        main_task.cancel()
        loop.run_until_complete(main_task)
    finally:
        loop.close()
        print('exit.')


if __name__ == '__main__':
    main()
