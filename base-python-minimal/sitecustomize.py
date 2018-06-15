import builtins
import socket

input_host = '127.0.0.1'
input_port = 65000


def _input(prompt=''):
    print(prompt, end='', flush=True)
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        try:
            sock.connect((input_host, input_port))
            userdata = sock.recv(1024)
        except ConnectionRefusedError:
            userdata = b'<user-input-unavailable>'
    return userdata.decode()


builtins._input = input
builtins.input = _input
