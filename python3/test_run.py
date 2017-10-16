import argparse
from getpass import getpass
import json
import sys
import textwrap

import zmq
import colorama
from colorama import Fore


def execute(code_type, code_text):
    ctx = zmq.Context.instance()
    ctx.setsockopt(zmq.LINGER, 50)
    if code_type == 'interrupt':
        repl_in = ctx.socket(zmq.PUSH)
        repl_in.connect('tcp://127.0.0.1:2000')
        repl_in.send_multipart([b'interrupt', b''])
        return
    repl_in = ctx.socket(zmq.PUSH)
    repl_in.connect('tcp://127.0.0.1:2000')
    repl_out = ctx.socket(zmq.PULL)
    repl_out.connect('tcp://127.0.0.1:2001')
    with repl_in, repl_out:
        msg = (code_type.encode('ascii'), code_text.encode('utf8'))
        repl_in.send_multipart(msg)
        while True:
            data = repl_out.recv_multipart()
            msg_type = data[0].decode('ascii')
            msg_data = data[1].decode('utf8')

            if msg_type == 'finished':
                print('--- finished ---')
                break
            elif msg_type == 'completion':
                print(msg_data)
                break
            elif msg_type == 'stdout':
                print(msg_data, end='')
                sys.stdout.flush()
            elif msg_type == 'stderr':
                print(Fore.RED + msg_data + Fore.RESET, end='', file=sys.stderr)
                sys.stderr.flush()
            elif msg_type == 'waiting-input':
                opts = json.loads(msg_data)
                if opts['is_password']:
                    t = getpass(prompt='')
                else:
                    t = input()
                repl_in.send_multipart([b'input', t.encode('utf8')])
            else:
                print('--- other msg ---')
                print(msg_type)
                print(msg_data)

sources = {
    'interleaving': ('code', '''
import sys
print('asdf', end='', file=sys.stderr)
print('qwer', end='', file=sys.stdout)
print('zxcv', file=sys.stderr)
'''),
    'exception': ('code', '''
raise RuntimeError("ooops")
'''),
    'long_running': ('code', '''
import time
for i in range(10):
    time.sleep(1)
    print(i)
'''),
    'user_input': ('code', '''
import hashlib
import getpass
print('Please type your name.')
name = input('>> ')
print(f'Hello, {name}')
print('Please type your password.')
pw = getpass.getpass()
m = hashlib.sha256()
m.update(pw.encode('utf8'))
print(f'Your password hash is {m.hexdigest()}')
'''),
    'early_exception': ('code', '''a = wrong-+****syntax'''),
    'runtime_error': ('code', '''
def x():
    raise RuntimeError('asdf')
def s():
    x()
if __name__ == '__main__':
    s()
'''),
    'completion': ('complete', json.dumps({
        'line': 'import pathlib; p',
    })),
    'interrupt': ('interrupt', ''),
}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('program_name')
    args = parser.parse_args()
    code_type, code_text = sources[args.program_name]
    print(f'Test {code_type}:')
    print(textwrap.indent(code_text, '  '))
    print('Execution log:')
    execute(code_type, code_text)


if __name__ == '__main__':
    colorama.init()
    main()
