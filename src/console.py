"""
***  IVY Language Interpreter
***  Ivy Console
"""

import sys
import argparse
from src.error import Error
from src.tracestack import SystemTrace
from src.lexer import Lexer
from src.parser import Parser
from src.resolver import Resolver
from src.interpreter import Interpreter
from src.files import IOWrapper

def write(text):
    sys.stderr.write(text)

def compile(filepath):
    trace = SystemTrace()
    lexer = Lexer(trace=trace)
    parser = Parser(trace=trace)
    interpreter = Interpreter(trace)
    resolver = Resolver(interpreter)
    try:
        io = IOWrapper(trace)
        io.frompath(filepath)
        file = io.newfile()
        tokens = lexer.tokenizefile(file)
        tree = parser.parse(file, tokens)
        resolver.resolve(file, tree)
        res = interpreter.interpret(file, tree)
        # print(locals)
    except Error as e:
        print(e)

def repl():
    trace = SystemTrace()
    lexer = Lexer(trace=trace)
    parser = Parser(trace=trace)
    interpreter = Interpreter(trace, console_mode=True)
    write("(c) Altun Hasanli\n")
    write("Ivy Language REPL.\n")
    while True:
        try:
            getin = input('>>> ').strip();
            io = IOWrapper(trace)
            io.fromstr(getin)
            file = io.newfile()
            tokens = lexer.tokenizefile(file)
            tree = parser.parse(file, tokens)
            resolver = Resolver(interpreter)
            resolver.resolve(file, tree)
            res = interpreter.interpret(file, tree)
        except Exception as e:
            sys.stderr.write(str(e) + '\n')
            trace.clear()

def main():
    """ Define the arguments for the console """
    parser = argparse.ArgumentParser(description='Ivy Language Console')
    parser.add_argument('-f', '--file', default=False, help='Execute Ivy Source File')
    args = parser.parse_args()

    """ Execute system commands """
    if len(sys.argv) == 1:
        return repl()
    if args.file:
        return compile(args.file)
    parser.print_help(sys.stderr)
    sys.exit(1)

if __name__ == '__main__':
    main()
