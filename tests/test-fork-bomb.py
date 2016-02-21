#! /usr/bin/env python3

# WARNING: this should be executed using sorna jail.

import os
import time
import sys

def bomb():
    n = 0
    pids = []
    while True:
        try:
            pid = os.fork()
            if pid == 0:
                # children
                time.sleep(1)
                sys.exit(0)
            else:
                n += 1
                pids.append(pid)
                continue
        except Exception as e:
            # The jail is expected to stop forking
            # by raising PermissionError on os.fork()
            print(e)
            break
    print('could create at most {} children'.format(n))
    for p in pids:
        # Prevent zombie processes.
        os.waitpid(-1, 0)

if __name__ == '__main__':
    bomb()
    time.sleep(2)
    # Repeat the test once more to check if
    # sorna jail correctly tracks the number of
    # currently running child processes.
    bomb()
