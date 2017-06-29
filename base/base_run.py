#! /usr/in/env python3

from abc import ABC, abstractmethod
import asyncio
import logging
import os
import signal
import sys

import aiozmq
import uvloop
import zmq

log = logging.getLogger()


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


class BaseRun(ABC):

    insock = None
    outsock = None
    child_env = None

    def __init__(self):
        super().__init__()

    @abstractmethod
    async def build(self, build_cmd):
        """Process build step."""

    @abstractmethod
    async def execute(self, exec_cmd):
        """Process execute step."""

    @abstractmethod
    async def query(self, code_text):
        """Run user code by creating a temporary file and compiling it."""

    def set_child_env(self, env=None):
        if not env:
            log.info('subprocess environment variables are not set')
            env = dict()
        self.child_env = env

    async def run_subproc(self, cmd):
        """A thin wrapper for an external command."""
        loop = asyncio.get_event_loop()
        try:
            proc = await asyncio.create_subprocess_shell(
                cmd,
                env=self.child_env,
                stdin=None,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            pipe_tasks = [
                loop.create_task(pipe_output(proc.stdout, self.outsock, 'stdout')),
                loop.create_task(pipe_output(proc.stderr, self.outsock, 'stderr')),
            ]
            await proc.wait()
            for t in pipe_tasks:
                t.cancel()
                await t
        except:
            log.exception('unexpected error')

    async def main_loop(self):
        insock = await aiozmq.create_zmq_stream(zmq.PULL, bind='tcp://*:2000')
        outsock = await aiozmq.create_zmq_stream(zmq.PUSH, bind='tcp://*:2001')
        outsock.write([b'stdout', b'akdj;fajwe;f\n'])
        self.insock, self.outsock = insock, outsock
        self.set_child_env()
        print('start serving...')
        while True:
            try:
                data = await insock.read()
                op_type = data[0].decode('ascii')
                text = data[1].decode('utf8')
                if op_type == 'build':  # batch-mode step 1
                    await self.build(text)
                    outsock.write([b'build-finished', b''])
                elif op_type == 'exec':  # batch-mode step 2
                    await self.execute(text)
                    outsock.write([b'finished', b''])
                elif op_type == 'input':  # query-mode
                    await self.query(text)
                    outsock.write([b'finished', b''])
                await outsock.drain()
            except asyncio.CancelledError:
                break
            except:
                log.exception('unexpected error')
                break
        insock.close()
        outsock.close()

    def run(self):
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
            main_task = loop.create_task(self.main_loop())
            loop.run_forever()
            # interrupted
            main_task.cancel()
            loop.run_until_complete(main_task)
        finally:
            loop.close()
            print('exit.')
