#! /usr/bin/env python3

import argparse
import asyncio
import fcntl
import io
import os
from pathlib import Path
import pty
import re
import shlex
import signal
import struct
import subprocess
import sys
import termios
import traceback
import types

import aiozmq
json_opts = {}
try:
    import simplejson as json
    json_opts['namedtuple_as_object'] = False
except ImportError:
    import json
# import pygit2
# from pygit2 import GIT_SORT_TOPOLOGICAL, GIT_SORT_REVERSE
import uvloop
import zmq


class StdoutProtocol(asyncio.Protocol):

    def __init__(self, sock_term_out, runner):
        self.transport = None
        self.sock_term_out = sock_term_out
        self.runner = runner

    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        self.sock_term_out.write([data])

    def connection_lost(self, exc):
        if not self.runner.ev_term.is_set():
            print("shell exited, restarting it.")
            self.sock_term_out.write([b'Restarting the shell...\r\n'])
            os.waitpid(self.runner.pid, 0)
            asyncio.ensure_future(self.runner.start_shell(), loop=self.runner.loop)


class TerminalRunner(object):
    '''
    A thin wrapper for REPL.

    It creates a dummy module that user codes run and keeps the references to user-created objects
    (e.g., variables and functions).
    '''

    def __init__(self, ev_term, sock_in, sock_out, loop=None):
        self._sorna_media = []
        self.loop = loop if loop else asyncio.get_event_loop()

        self.ev_term = ev_term
        self.pid = None
        self.fd = None

        # For control commands
        self.sock_in  = sock_in
        self.sock_out = sock_out

        # For terminal I/O
        self.sock_term_in  = None
        self.sock_term_out = None

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

        parser_show = subparsers.add_parser('show')
        parser_show.add_argument('target', choices=('graph',), default='graph')
        parser_show.add_argument('path', type=str)
        parser_show.set_defaults(func=self.do_show)

    def do_ping(self, args):
        self.sock_out.write([b'stdout', b'pong!'])

    def do_chdir(self, args):
        os.chdir(args.path)
        self.sock_out.write([
            b'stdout',
            f'OK; changed working directory to {args.path}'.encode(),
        ])

    def do_resize_term(self, args):
        origsz = struct.pack('HHHH', 0, 0, 0, 0)
        origsz = fcntl.ioctl(self.fd, termios.TIOCGWINSZ, origsz)
        _, _, origx, origy = struct.unpack('HHHH', origsz)
        newsz = struct.pack('HHHH', args.rows, args.cols, origx, origy)
        newsz = fcntl.ioctl(self.fd, termios.TIOCSWINSZ, newsz)
        newr, newc, _, _ = struct.unpack('HHHH', newsz)
        self.sock_out.write([
            b'stdout',
            f'OK; terminal resized to {newr} rows and {newc} cols'.encode(),
        ])

    def do_show(self, args):
        if args.target == 'graph':
            commit_branch_table = {}
            commit_info = []

            if args.path in ['.', None]:
                current_dir = Path(f'/proc/{self.pid}/cwd').resolve()
            else:
                current_dir = Path(args.path).resolve()
            os.chdir(current_dir)

            # Create commit-branch matching table.
            tree_cmd = ['git', 'log', '--pretty=oneline', '--graph',
                        '--source', '--branches']
            run_res = subprocess.run(tree_cmd, stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
            stdout = run_res.stdout.decode('utf-8')
            stderr = run_res.stderr.decode('utf-8')
            prog = re.compile(r'([a-z0-9]+)\s+(\S+).*')
            if stderr:
                self.sock_out.write([b'stderr', stderr.encode('utf-8')])
                return

            for line in stdout.split('\n'):
                r = prog.search(line)
                if r and hasattr(r, 'group') and r.group(1) and r.group(2):
                    oid = r.group(1)[:7]  # short oid
                    branch = r.group(2)
                    commit_branch_table[oid] = branch

            # Gather commit info w/ branch name.
            log_cmd = ['git', 'log', '--pretty=format:%h||%p||%s||%cn',
                       '--all', '--topo-order', '--reverse']
            run_res = subprocess.run(log_cmd, stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
            stdout = run_res.stdout.decode('utf-8')
            for log in stdout.split('\n'):
                items = log.split('||')
                oid = items[0]
                parent_ids = items[1].split(' ')
                message = items[2]
                author = items[3]
                branch = commit_branch_table.get(oid, None)
                parent_branches = [commit_branch_table.get(pid, None)
                                   for pid in parent_ids]
                info = dict(
                    oid=oid,
                    parent_ids=parent_ids,
                    author=author,
                    message=message,
                    branch=branch,
                    parent_branches=parent_branches
                )
                commit_info.append(info)

            self.sock_out.write([
                b'media',
                json.dumps({
                    'type': 'application/vnd.sorna.gitgraph',
                    'data': commit_info
                }).encode('utf-8')
            ])
        else:
            raise ValueError('Unsupported show target', args.target)

    async def handle_command(self, code_id, code_txt):
        try:
            if code_txt.startswith('%'):
                args = self.cmdparser.parse_args(shlex.split(code_txt[1:], comments=True))
                if asyncio.iscoroutine(args.func) or asyncio.iscoroutinefunction(args.func):
                    await args.func(args)
                else:
                    args.func(args)
            else:
                self.sock_out.write([b'stderr', b'Invalid command.'])
        except:
            exc_type, exc_val, tb = sys.exc_info()
            trace = traceback.format_exception(exc_type, exc_val, tb)
            self.sock_out.write([b'stderr', trace.encode()])
        finally:
            opts = {
                'upload_output_files': False,
            }
            self.sock_out.write([b'finished', json.dumps(opts).encode()])

    async def start_shell(self):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.execv('/bin/bash', ['/bin/bash'])
        else:
            if self.sock_term_in is None:
                self.sock_term_in  = await aiozmq.create_zmq_stream(zmq.SUB, bind='tcp://*:2002')
                self.sock_term_in.transport.subscribe(b'')
            if self.sock_term_out is None:
                self.sock_term_out = await aiozmq.create_zmq_stream(zmq.PUB, bind='tcp://*:2003')
            await self.loop.connect_read_pipe(lambda: StdoutProtocol(self.sock_term_out, self),
                                              os.fdopen(self.fd, 'rb'))
            asyncio.ensure_future(self.terminal_in())
            print('opened shell pty: stdin at port 2002, stdout at port 2003')

    async def terminal_in(self):
        while True:
            try:
                data = await self.sock_term_in.read()
            except aiozmq.ZmqStreamClosed:
                break
            try:
                os.write(self.fd, data[0])
            except OSError:
                break

    async def kill_shell(self):
        self.sock_term_in.close()
        self.sock_term_out.close()
        os.kill(self.pid, signal.SIGHUP)
        os.kill(self.pid, signal.SIGCONT)
        await asyncio.sleep(0)
        ret = os.waitpid(self.pid, 0)
        self.pid = None
        self.fd = None
        print('killed shell')


async def repl(ev_term, sock_in, sock_out):
    runner = TerminalRunner(ev_term, sock_in, sock_out)
    await runner.start_shell()
    try:
        while True:
            try:
                data = await sock_in.read()
                result = await runner.handle_command(data[0].decode(),
                                                     data[1].decode())
            except (aiozmq.ZmqStreamClosed, asyncio.CancelledError):
                break
            await sock_out.drain()
    except asyncio.CancelledError:
        pass
    finally:
        await runner.kill_shell()

def main():
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()

    sock_in  = None
    sock_out = None
    repl_task = None
    ev_term = asyncio.Event()

    def signal_handler(loop, ev_term):
        if not ev_term.is_set():
            loop.stop()

    loop.add_signal_handler(signal.SIGTERM, signal_handler, loop, ev_term)
    loop.add_signal_handler(signal.SIGINT, signal_handler, loop, ev_term)

    async def init():
        nonlocal sock_in, sock_out, repl_task
        sock_in  = await aiozmq.create_zmq_stream(zmq.PULL, bind='tcp://*:2000')
        sock_out = await aiozmq.create_zmq_stream(zmq.PUSH, bind='tcp://*:2001')
        repl_task = loop.create_task(repl(ev_term, sock_in, sock_out))

    async def shutdown():
        repl_task.cancel()
        await repl_task
        sock_in.close()
        sock_out.close()
        await asyncio.sleep(0)

    try:
        loop.run_until_complete(init())
        loop.run_forever()
        # interrupted
        ev_term.set()
        loop.run_until_complete(shutdown())
    finally:
        print('exit.')
        loop.close()

if __name__ == '__main__':
    main()
