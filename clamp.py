#!/bin/env python3

from subprocess import Popen, PIPE, STDOUT
from sys import stdin

def main():
    cmd = ["/usr/bin/sbcl"]
    bufsize = 0
    p = Popen(cmd, shell=False, bufsize=bufsize, stdin=PIPE, stdout=PIPE, stderr=PIPE,
              text=True, encoding="UTF-8", close_fds=True)
    (child_stdin, child_stdout, child_stderr) = (p.stdin, p.stdout, p.stderr)

    running = True
    while running:
        input = stdin.readline().strip()
        if input == 'quit':
            running = False
            child_stdin.write("(quit)\n")
            child_stdin.flush()
        else:
            child_stdin.write(input)
            child_stdin.write('\n')
            child_stdin.flush()

    output = child_stdout.readlines()
    [print(l.strip() + '\n') for l in output]
    print()

if __name__ == '__main__':
    main()

