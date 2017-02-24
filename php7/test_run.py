import argparse
from getpass import getpass
import json
import sys
import textwrap

import zmq
import colorama
from colorama import Fore


def execute(code):
    ctx = zmq.Context.instance()
    ctx.setsockopt(zmq.LINGER, 50)
    repl_in = ctx.socket(zmq.PUSH)
    repl_in.connect('tcp://127.0.0.1:2000')
    repl_out = ctx.socket(zmq.PULL)
    repl_out.connect('tcp://127.0.0.1:2001')
    with repl_in, repl_out:
        msg = (b'xcode1', code.encode('utf8'))
        repl_in.send_multipart(msg)
        while True:
            data = repl_out.recv_multipart()
            msg_type = data[0].decode('ascii')
            msg_data = data[1].decode('utf8')

            if msg_type == 'finished':
                print('--- finished ---')
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
    'echo': r'''
echo "asdfasdfadfs\n";
echo "asjdfoija\naosihgoi\n";
echo "한글도 잘 될까\n";
''',
    'user_input': r'''
$s = ($this->input)('prompt> ');
echo $s;
''',
    'early_exception': '''a = wrong-+****syntax''',
    'runtime_error': '''
throw new Exception('oops');
''',
}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('program_name')
    args  =parser.parse_args()
    src = sources[args.program_name]
    print('Test code:')
    print(textwrap.indent(src, '  '))
    print()
    print('Execution log:')
    execute(src)


if __name__ == '__main__':
    colorama.init()
    main()
