#! /usr/in/env python3

from abc import ABC, abstractmethod
import asyncio
import logging
import logging.config
from logging.handlers import QueueHandler
import os
import signal
import sys

import aiozmq
import uvloop
import zmq

log = logging.getLogger()


class OutsockHandler(QueueHandler):
    def enqueue(self, record):
        msg = self.formatter.format(record)
        self.queue.write([
            b'stderr',
            (msg + '\n').encode('utf8'),
        ])


class BraceLogRecord(logging.LogRecord):
    def getMessage(self):
        if self.args is not None:
            return self.msg.format(*self.args)
        return self.msg


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


class BaseRunner(ABC):

    log_prefix = 'generic-kernel'

    def __init__(self):
        self.child_env = {}
        self.insock = None
        self.outsock = None

    @abstractmethod
    async def build(self, build_cmd):
        """Process build step."""

    @abstractmethod
    async def execute(self, exec_cmd):
        """Process execute step."""

    @abstractmethod
    async def query(self, code_text):
        """Run user code by creating a temporary file and compiling it."""

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

    async def handle_user_input(self, reader, writer):
        try:
            self.outsock.write([b'waiting-input', b''])
            data = await self.insock.read()
            user_input = data[1]
            writer.write(user_input)
            await writer.drain()
            writer.close()
        except:
            log.exception('unexpected error (handle_user_input)')

    async def main_loop(self):
        self.insock = await aiozmq.create_zmq_stream(zmq.PULL, bind='tcp://*:2000')
        self.outsock = await aiozmq.create_zmq_stream(zmq.PUSH, bind='tcp://*:2001')
        user_input_server = \
            await asyncio.start_server(self.handle_user_input, '127.0.0.1', 65000)

        # configure logging to publish logs via outsock as well
        logging.basicConfig(
            level=logging.DEBUG,  # NOTE: change this to DEBUG when debugging
            format=self.log_prefix + ': {message}',
            style='{',
            handlers=[logging.StreamHandler(), OutsockHandler(self.outsock)],
        )
        _factory = lambda *args, **kwargs: BraceLogRecord(*args, **kwargs)
        logging.setLogRecordFactory(_factory)

        log.debug('start serving...')
        while True:
            try:
                data = await self.insock.read()
                op_type = data[0].decode('ascii')
                text = data[1].decode('utf8')
                if op_type == 'build':  # batch-mode step 1
                    await self.build(text)
                    self.outsock.write([b'build-finished', b''])
                elif op_type == 'exec':  # batch-mode step 2
                    await self.execute(text)
                    self.outsock.write([b'finished', b''])
                elif op_type == 'input':  # query-mode
                    await self.query(text)
                    self.outsock.write([b'finished', b''])
                await self.outsock.drain()
            except asyncio.CancelledError:
                break
            except:
                log.exception('unexpected error')
                break
        user_input_server.close()
        await user_input_server.wait_closed()
        self.insock.close()
        self.outsock.close()

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
                log.warning('forced shutdown!', file=sys.stderr)
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
            log.debug('exit.')
