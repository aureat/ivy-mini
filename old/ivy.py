"""
***  IVY Language Interpreter
***  Ivy Console
"""

import sys
import argparse
from src.system import System
from src.error import Error

class IvyConsole:
    def __init__(self, writer):
        self.writer = writer
        self._system = System()

    def write(self, text):
        self.writer.write(text)

    def main(self):
        """ Define the arguments for the console """
        parser = argparse.ArgumentParser(description='Ivy Language Console')
        parser.add_argument('-f', '--file', default=False, help='Execute Ivy Source File')
        parser.add_argument('-p', '--repl', default=False, action='store_true', help='Ivy Language Read-Eval-Print loop')
        parser.add_argument('-t', '--tokenize', default=False, help='Tokenize a source file')
        args = parser.parse_args()

        """ Execute system commands """
        if len(sys.argv) == 1:
            return self.repl()
        if args.file:
            return self._system.runfile(args.file)
        if args.tokenize:
            return self._system.tokenized(args.tokenize)
        if args.repl:
            return self.repl()
        parser.print_help(sys.stderr)
        sys.exit(1)

    def repl(self):
        self.write("Ivy Language REPL.\n")
        self._system.repl()
        while True:
            try:
                self.write(">>> "); getin = input();
                self._system.run_code(getin.strip())
            except Error as e:
                print(e)

if __name__ == '__main__':
    ivy = IvyConsole(sys.stderr)
    ivy.main()
