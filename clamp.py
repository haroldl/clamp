#!/bin/env python3

from subprocess import Popen, PIPE, STDOUT

def main():
    print("Hello, clamp")
    cmd = ["/usr/bin/sbcl"]
    cmd = ["/usr/bin/ls", "/home"]
    bufsize = 1024
    p = Popen(cmd, shell=False, bufsize=bufsize, stdin=PIPE, stdout=PIPE, stderr=PIPE,
              text=True, encoding="UTF-8", close_fds=True)
    (child_stdin, child_stdout, child_stderr) = (p.stdin, p.stdout, p.stderr)
    output = child_stdout.readlines()
    [print(l.strip()) for l in output]
    print()

if __name__ == '__main__':
    main()

