#! /usr/bin/env python
import io
from namedlist import namedtuple, namedlist
import os
import subprocess
import sys
import uuid
import zmq
try:
    import simplejson
    has_simplejson = True
except ImportError:
    has_simplejson = False

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
])


@staticmethod
def _create_excinfo(e, raised_before_exec, tb):
    assert isinstance(e, Exception)
    return ExceptionInfo(type(e).__name__, e.args, raised_before_exec, tb)
ExceptionInfo.create = _create_excinfo


class SockWriter(object):
    def __init__(self, sock, cell_id):
        self.cell_id_encoded = '{0}'.format(cell_id).encode('ascii')
        self.sock = sock
        self.buffer = io.StringIO()

    def write(self, s):
        if '\n' in s:  # flush on occurrence of a newline.
            s1, s2 = s.split('\n', maxsplit=1)
            s0 = self.buffer.getvalue()
            self.sock.send_multipart([self.cell_id_encoded, (s0 + s1 + '\n').encode('utf8')])
            self.buffer.seek(0)
            self.buffer.truncate(0)
            self.buffer.write(s2)
        else:
            self.buffer.write(s)
        if self.buffer.tell() > 1024:  # flush if the buffer is too large.
            s0 = self.buffer.getvalue()
            self.sock.send_multipart([self.cell_id_encoded, s0.encode('utf8')])
            self.buffer.seek(0)
            self.buffer.truncate(0)
        # TODO: timeout to flush?


class CodeRunner(object):
    '''
    A thin wrapper for haskell compile & runner.

    It creates a temporary file with user haskell code, run it with ``runhaskell``, and
    returns the outputs of the execution.
    '''
    def execute(self, cell_id, src):
        # TODO: exception handling. needed?
        exceptions = []
        result = Result()
        before_exec = True

        def my_excepthook(type_, value, tb):
            exceptions.append(ExceptionInfo.create(value, before_exec, tb))
        sys.excepthook = my_excepthook

        # Save haskell code to a temporary file
        tmp_fname = "./tmp-code-{}.hs".format(str(uuid.uuid4()))
        with open(tmp_fname, 'w') as f:
            f.write(src)

        # Compile and run the saved haskell code.
        p = subprocess.run("runhaskell {}".format(tmp_fname), shell=True,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        result.stdout = p.stdout.decode("utf-8") if p.stdout else ''
        result.stderr = p.stderr.decode("utf-8") if p.stderr else ''

        # Delete temporary haskell codes
        if os.path.exists(tmp_fname):
            os.remove(tmp_fname)

        sys.excepthook = sys.__excepthook__

        return exceptions, result


if __name__ == '__main__':
    # Use the "confined" working directory
    os.chdir('/home/work')
    # Replace stdin with a "null" file
    # (trying to read stdin will raise EOFError immediately afterwards.)
    sys.stdin = open(os.devnull, 'rb')

    # Initialize context object.
    runner = CodeRunner()

    # Initialize minimal ZMQ server socket.
    ctx = zmq.Context(io_threads=1)
    sock = ctx.socket(zmq.REP)
    sock.bind('tcp://*:2001')
    print('serving at port 2001...')

    try:
        while True:
            data = sock.recv_multipart()
            exceptions, result = runner.execute(data[0].decode('ascii'),
                                                data[1].decode('utf8'))
            response = {
                'stdout': result.stdout,
                'stderr': result.stderr,
                'media': result.media,
                'exceptions': exceptions,
            }
            json_opts = {}
            if has_simplejson:
                json_opts['namedtuple_as_object'] = False
            sock.send_json(response, **json_opts)
    except (KeyboardInterrupt, SystemExit):
        pass
    finally:
        sock.close()
        print('exit.')
