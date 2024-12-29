#!/bin/env python

"""Driver to compile Python to Common Lisp and print out the generated code.
"""

from clamp_compiler import *


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("You must specify the file name for the Python source to translate to Lisp.")
        sys.exit(1)

    for i in range(len(sys.argv)):
        print(i, sys.argv[i])

    with open(sys.argv[1]) as source_file:
        source = source_file.read()
        output = compile(source)
        print(output)
