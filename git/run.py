#! /usr/bin/env python3

import argparse
import asyncio
import io
import os
import pty
import shlex
import signal
import subprocess
import sys
import types

import aiozmq
from namedlist import namedtuple, namedlist
try:
    import simplejson
    has_simplejson = True
except ImportError:
    has_simplejson = False
import pygit2
import uvloop
import zmq


ExceptionInfo = namedtuple('ExceptionInfo', [
    'exc',
    ('args', tuple()),
    ('raised_before_exec', False),
    ('traceback', None),
])

Result = namedlist('Result', [
    ('stdout', ''),
    ('stderr', ''),
    ('media', None),
    ('options', None),
])


class StdoutProtocol(asyncio.Protocol):

    transport = None
    sock_out = None

    def __init__(self, sock_out):
        self.sock_out = sock_out

    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        self.sock_out.write([data])
        print('terminal output:', data)

    def connection_lost(self, exc):
        pass


class TerminalRunner(object):
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self, loop):
        self._sorna_media = []
        self.loop = loop
        self.pid = None
        self.fd = None

        self.cmdparser = argparse.ArgumentParser()
        subparsers = self.cmdparser.add_subparsers()

        parser_chdir = subparsers.add_parser('chdir')
        parser_chdir.add_argument('path', type=str)
        parser_chdir.set_defaults(func=self.do_chdir)

        parser_show = subparsers.add_parser('show')
        parser_show.add_argument('target', choices=('graph',), default='graph')
        parser_show.add_argument('path', type=str)
        parser_show.set_defaults(func=self.do_show)

    def do_chdir(self, args):
        os.chdir(args.path)

    def do_show(self, args):
        if args.target == 'graph':
            repo_path = pygit2.discover_repository(args.path)
            repo = pygit2.Repository(repo_path)
            for b in repo.listall_branches():
                print('Branch: {}'.format(b))
                branch = repo.lookup_branch(b)
                for log in branch.log():
                    print('  {}'.format(log.oid_new))
        else:
            raise ValueError('Unsupported show target', args.target)

    async def handle_command(self, cell_id, src):
        args = self.cmdparser.parse(src)
        args.func(args)

    async def start_shell(self):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.execv('/bin/bash', ['/bin/bash'])
        else:
            self.sock_in  = await aiozmq.create_zmq_stream(zmq.SUB, bind='tcp://*:2002')
            self.sock_in.transport.subscribe(b'')
            self.sock_out = await aiozmq.create_zmq_stream(zmq.PUB, bind='tcp://*:2003')
            await loop.connect_read_pipe(lambda: StdoutProtocol(self.sock_out),
                                         os.fdopen(self.fd, 'rb'))
            loop.create_task(self.terminal_in())
            print('opened shell pty: stdin at port 2002, stdout at port 2003')

    async def terminal_in(self):
        while True:
            try:
                data = await self.sock_in.read()
            except aiozmq.ZmqStreamClosed:
                break
            try:
                os.write(self.fd, data[0])
                print('terminal input:', data[0])
            except OSError:
                break

    def kill_shell(self):
        self.sock_in.close()
        self.sock_out.close()
        os.close(self.fd)
        os.kill(self.pid, signal.SIGHUP)
        os.kill(self.pid, signal.SIGCONT)
        ret = os.waitpid(self.pid)
        self.pid = None
        self.fd = None
        print('killed shell')


async def repl(sock, runner):

    json_opts = {}
    if has_simplejson:
        json_opts['namedtuple_as_object'] = False

    await runner.start_shell()
    try:
        while True:
            try:
                data = await sock.read()
                result = await runner.handle_command(data[0].decode('ascii'),
                                                     data[1].decode('utf8'))
            except (aiozmq.ZmqStreamClosed, asyncio.CancelledError):
                break
            response = {
                'stdout': result.stdout,
                'stderr': '',
                'media': [],
                'options': {'upload_output_files': False},
                'exceptions': [],
            }
            sock.send_json(response, **json_opts)
    finally:
        runner.kill_shell()

def terminate():
    raise SystemExit


if __name__ == '__main__':
    #asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()
    sock_repl = loop.run_until_complete(
        aiozmq.create_zmq_stream(zmq.REP, bind='tcp://*:2001', loop=loop))
    loop.add_signal_handler(signal.SIGTERM, terminate)
    loop.add_signal_handler(signal.SIGINT, terminate)
    print('serving at port 2001...')

    runner = TerminalRunner(loop)
    task = loop.create_task(repl(sock_repl, runner))
    try:
        loop.run_forever()
    except (KeyboardInterrupt, SystemExit):
        sock_repl.close()
        loop.stop()
    finally:
        print('exit.')
        loop.close()
