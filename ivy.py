"""
***  IVY Language Interpreter
***  Main File

(c) Altun Hasanli 2019

"""
import sys
import argparse
from src.system import System
from src.error import Error

"""
*** IVY CONSOLE
"""

class IvyConsole:
    def __init__(self):
        self._system = System()

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
        """ The REPL """
        sys.stderr.write("Ivy Language REPL.\n")
        self._system.repl()
        while True:
            try:
                getin = input("ivy> ")
                self._system.run_code(str(getin))
            except Exception as e:
                if isinstance(e, Error): print(e)
                else: print("PythonError: " + str(e))

if __name__ == '__main__':
    ivy = IvyConsole()
    ivy.main()
