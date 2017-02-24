#! /usr/bin/env python
import asyncio
import io
import logging
import os
import signal
import sys
import tempfile

from namedlist import namedtuple, namedlist
import simplejson as json
import uvloop
import zmq, aiozmq


log = logging.getLogger()

cmdspec = 'runhaskell {mainpath}'


'''
A thin wrapper for an external command.

It creates a temporary file with user haskell code, run it with ``runhaskell``, and
returns the outputs of the execution.
'''
async def execute(insock, outsock, code_id, code_data):
    loop = asyncio.get_event_loop()

    # Save haskell code to a temporary file
    tmpf = tempfile.NamedTemporaryFile()
    tmpf.write(code_data)
    tmpf.flush()

    try:
        # Compile and run the saved haskell code.
        proc = await asyncio.create_subprocess_shell(cmdspec.format(mainpath=tmpf.name),
            stdin=None, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE)
        pipe_tasks = [
            loop.create_task(pipe_output(proc.stdout, outsock, 'stdout')),
            loop.create_task(pipe_output(proc.stderr, outsock, 'stderr')),
        ]
        await proc.wait()
        for t in pipe_tasks:
            t.cancel()
            await t
    except:
        log.exception()
    finally:
        # Close and delete the temporary file.
        tmpf.close()


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

async def main_loop():
    insock  = await aiozmq.create_zmq_stream(zmq.PULL, bind='tcp://*:2000')
    outsock = await aiozmq.create_zmq_stream(zmq.PUSH, bind='tcp://*:2001')
    print('start serving...')
    while True:
        try:
            data = await insock.read()
            code_id = data[0].decode('ascii')
            code_data = data[1]
            await execute(insock, outsock, code_id, code_data)
            outsock.write([b'finished', b''])
            await outsock.drain()
        except asyncio.CancelledError:
            break
        except:
            log.exception()
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
