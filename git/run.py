#! /usr/bin/env python3

import argparse
import asyncio
import fcntl
import io
import os
import pty
import shlex
import signal
import struct
import subprocess
import sys
import termios
import types

import aiozmq
from namedlist import namedtuple, namedlist, FACTORY
json_opts = {}
try:
    import simplejson as json
    json_opts['namedtuple_as_object'] = False
except ImportError:
    import json
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
    ('media', FACTORY(list)),
    ('options', FACTORY(dict)),
])


class StdoutProtocol(asyncio.Protocol):

    transport = None
    sock_out = None

    def __init__(self, sock_out, runner):
        self.sock_out = sock_out
        self.runner = runner

    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        self.sock_out.write([data])

    def connection_lost(self, exc):
        if not self.runner.ev_term.is_set():
            print("shell exited, restarting it.")
            self.sock_out.write([b'Restarting the shell...\r\n'])
            asyncio.ensure_future(self.runner.start_shell(), loop=self.runner.loop)
        os.waitpid(self.runner.pid, 0)


class TerminalRunner(object):
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self, loop, ev_term):
        self._sorna_media = []
        self.loop = loop
        self.ev_term = ev_term
        self.pid = None
        self.fd = None
        self.sock_in = None
        self.sock_out = None

        self.cmdparser = argparse.ArgumentParser()
        subparsers = self.cmdparser.add_subparsers()

        parser_ping = subparsers.add_parser('ping')
        parser_ping.set_defaults(func=self.do_ping)

        parser_resize = subparsers.add_parser('resize')
        parser_resize.add_argument('rows', type=int)
        parser_resize.add_argument('cols', type=int)
        parser_resize.set_defaults(func=self.do_resize_term)

        parser_chdir = subparsers.add_parser('chdir')
        parser_chdir.add_argument('path', type=str)
        parser_chdir.set_defaults(func=self.do_chdir)

        # TODO: Temporarily disabled.
        #parser_show = subparsers.add_parser('show')
        #parser_show.add_argument('target', choices=('graph',), default='graph')
        #parser_show.add_argument('path', type=str)
        #parser_show.set_defaults(func=self.do_show)

    def do_ping(self, args):
        return Result('pong!', '')

    def do_chdir(self, args):
        os.chdir(args.path)
        return Result('changed working directory to {}'.format(args.path), '')

    def do_resize_term(self, args):
        origsz = struct.pack('HHHH', 0, 0, 0, 0)
        origsz = fcntl.ioctl(self.fd, termios.TIOCGWINSZ, origsz)
        _, _, origx, origy = struct.unpack('HHHH', origsz)
        newsz = struct.pack('HHHH', args.rows, args.cols, origx, origy)
        newsz = fcntl.ioctl(self.fd, termios.TIOCSWINSZ, newsz)
        newr, newc, _, _ = struct.unpack('HHHH', newsz)
        return Result('OK; terminal resized to {} rows and {} cols'.format(newr, newc), '')

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
        if src.startswith('%'):
            args = self.cmdparser.parse_args(shlex.split(src[1:], comments=True))
            if asyncio.iscoroutine(args.func) or asyncio.iscoroutinefunction(args.func):
                return (await args.func(args))
            else:
                return args.func(args)
        else:
            return Result('', 'Invalid command.')

    async def start_shell(self):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.execv('/bin/bash', ['/bin/bash'])
        else:
            if self.sock_in is None:
                self.sock_in  = await aiozmq.create_zmq_stream(zmq.SUB, bind='tcp://*:2002')
                self.sock_in.transport.subscribe(b'')
            if self.sock_out is None:
                self.sock_out = await aiozmq.create_zmq_stream(zmq.PUB, bind='tcp://*:2003')
            await loop.connect_read_pipe(lambda: StdoutProtocol(self.sock_out, self),
                                         os.fdopen(self.fd, 'rb'))
            asyncio.ensure_future(self.terminal_in())
            print('opened shell pty: stdin at port 2002, stdout at port 2003')

    async def terminal_in(self):
        while True:
            try:
                data = await self.sock_in.read()
            except aiozmq.ZmqStreamClosed:
                break
            try:
                os.write(self.fd, data[0])
            except OSError:
                break

    def kill_shell(self):
        self.sock_in.close()
        self.sock_out.close()
        os.kill(self.pid, signal.SIGHUP)
        os.kill(self.pid, signal.SIGCONT)
        ret = os.waitpid(self.pid, 0)
        self.pid = None
        self.fd = None
        print('killed shell')


async def repl(sock, runner):
    await runner.start_shell()
    try:
        while True:
            try:
                data = await sock.read()
                result = await runner.handle_command(data[0].decode(),
                                                     data[1].decode())
            except (aiozmq.ZmqStreamClosed, asyncio.CancelledError):
                break
            result.options['upload_output_files'] = False
            response = {
                'stdout': result.stdout,
                'stderr': result.stderr,
                'media': result.media,
                'options': result.options,
                'exceptions': [],
            }
            sock.write([json.dumps(response, **json_opts).encode()])
            await sock.drain()
    finally:
        runner.kill_shell()

def signal_handler(loop, ev_term):
    if not ev_term.is_set():
        loop.stop()


if __name__ == '__main__':
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()
    sock_repl = loop.run_until_complete(
        aiozmq.create_zmq_stream(zmq.REP, bind='tcp://*:2001', loop=loop))

    ev_term = asyncio.Event()
    loop.add_signal_handler(signal.SIGTERM, signal_handler, loop, ev_term)
    loop.add_signal_handler(signal.SIGINT, signal_handler, loop, ev_term)

    runner = TerminalRunner(loop, ev_term)
    asyncio.ensure_future(repl(sock_repl, runner))
    try:
        print('serving at port 2001...')
        loop.run_forever()
        ev_term.set()
        sock_repl.close()
        loop.run_until_complete(asyncio.sleep(0.05))
        loop.stop()
    finally:
        print('exit.')
        loop.close()
