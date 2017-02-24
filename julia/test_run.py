import argparse
from codecs import getincrementaldecoder
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
        decoder = getincrementaldecoder('utf8')()
        while True:
            data = repl_out.recv_multipart()
            msg_type = data[0].decode('ascii')
            msg_data = decoder.decode(data[1])

            if msg_type == 'finished':
                decoder.decode(b'', True)
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
    'interleaving': '''
println("adsf"); println("zcxb")
println("asdf")
''',
    'plot': '''
#using Plots
#unicodeplots()
#plot(rand(5,5),linewidth=2,title="My Plot")

using PyPlot
x = linspace(0,2*pi,1000); y = sin(3*x + 4*cos(2*x))
plot(x, y, color="red", linewidth=2.0, linestyle="--")
display("image/svg+xml", gcf())

#using Gadfly
#draw(SVG("output.svg", 6inch, 3inch), plot([sin, cos], 0, 25))

#using Winston
#figure(width=600, height=400)
#pl = plot(cumsum(rand(500) .- 0.5), "r", cumsum(rand(500) .- 0.5), "b")
#display("image/png", pl)
''',
    'table': '''
using DataFrames
df = DataFrame(A = [1, 2], B = [e, pi], C = ["xx", "xy"])
show(df)
display(df)
''',
    'user_input': '''
name = readprompt("your name? ")
println("hello, $name")
''',
    'early_exception': '''a = 1\na = wrong-+****syntax''',
    'runtime_error': '''error("ooops!")''',
}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('program_name')
    args  =parser.parse_args()
    src = sources[args.program_name]
    print('Test code:')
    print(textwrap.indent(src, '  '))
    print('Execution log:')
    execute(src)


if __name__ == '__main__':
    colorama.init()
    main()
