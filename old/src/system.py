"""
*** Ivy System
"""
import os
import sys
import argparse
from enum import Enum
from src.error import *
from src.parser import Parser
from src.interpreter import Interpreter
from src.objects import *
from src.callstack import *
from src.lexer import Lexer
from src.tokentype import TokenType
from src.ast import *
from src.tracestack import SystemTrace
from src.module import Module
from src.iomodule import IOModule

class IvyObj(object):
    def __init__(self, name, object, interpreter=None, trace=None):
        self.name = name
        self.object = object
        self.type = 'SystemObject'
        self.interpreter = interpreter
        self.trace = trace
        self.token = None
        self.objdef = AttributeObject({
            'name': name,
            'type': 'SystemObject',
        }, name='SystemObject', interpreter=interpreter, trace=trace)
        self.attributes = {
            'attrget': self.newmethod(self.attrget, ['attr'], 'attrget'),
            'attrset': self.newmethod(self.attrset, ['attr', 'value'], 'attrset'),
            'attrhas': self.newmethod(self.attrhas, ['attr'], 'attrhas'),
            'attrdel': self.newmethod(self.attrdel, ['attr'], 'attrdel'),
        }

    """ OBJECT METHODS """
    def newobj(self, obj, token=None):
        token = None
        if isinstance(obj, bool):
            return Boolean(obj, token, self.trace, self.interpreter)
        elif isinstance(obj, int):
            return Integer(obj, token, self.trace, self.interpreter)
        elif isinstance(obj, float):
            return Float(obj, token, self.trace, self.interpreter)
        elif isinstance(obj, str):
            return String(obj, token, self.trace, self.interpreter)
        elif obj == 'null':
            return Null(token, self.trace, self.interpreter)
        elif isinstance(obj, list):
            return Collection(obj, token, self.trace, self.interpreter)
        elif callable(obj):
            return self.newmethod(obj, [], obj.__name__)
        return obj

    def newivy(self, object):
        return IvyObj(self.name, object, self.interpreter, self.trace)

    def newmethod(self, func, params, name):
        return BuiltinMethod(boundclass=self, func=func, params=params, name=name, token=self.token, trace=self.trace, interpreter=self.interpreter)

    def getobjdef(self, name):
        return self.newobj(self.objdef[name])

    """ OBJECT ATTRIBUTES """
    def attrget(self, att):
        if att in self.attributes:
            return self.newobj(self.attributes[att])
        if att in self.object.__dict__:
            return self.newobj(self.object.__dict__[att])
        return Boolean(False)

    def attrset(self, att, val):
        if att in self.attributes:
            oldval = self.attributes[att]
            self.attributes[att] = self.newobj(val)
            return oldval
        if att in self.object.__dict__:
            oldval = self.object.__dict__[att]
            self.object.__dict__[att] = self.newobj(val)
            return oldval
        return Boolean(False)

    def attrdel(self, att):
        if att in self.attributes:
            del self.attributes[att]
        if att in self.object.__dict__:
            del self.object.__dict__[att]

    def attrhas(self, att):
        if att in self.attributes:
            return Boolean(True)
        if att in self.object.__dict__:
            return Boolean(True)
        return Boolean(False)

    """ TO STRING METHODS """
    def newprintable(self, string):
        return self.newobj(string)
    def representation(self):
        return self.newprintable("<SystemObject '%s' at 0x%08x>" % (self.name, id(self)))
    def printable(self):
        return self.newprintable(self.representation())
    def toprint(self):
        return self.representation().toprint()
    def __repr__(self):
        return self.toprint()
    def __str__(self):
        return self.toprint()

class SystemModule(Module):
    def __init__(self, system):
        super().__init__('SystemModule', system.trace, system.interpreter)
        # SYSTEM OBJECTS
        self.system = system
        self.lexer = system.lexer_obj
        self.parser = system.parser
        self.interpreter = system.interpreter
        self.trace = system.trace
        self.callstack = system.callstack_obj
        self.io = system.io
        self.objmachine = system.objmachine
        # ATTRIBUTES
        self.attributes['new'] = self.newmethod(self.newobj, ['obj'], 'new')
        self.attributes['stdin'] = self.io.stdin
        self.attributes['stdout'] = self.io.stdout
        self.attributes['stderr'] = self.io.stderr
        self.attributes['IO'] = self.io
        self.attributes['CallStack'] = self.callstack
        self.attributes['Lexer'] = self.lexer

    def channel_in(self, file):
        self.io.stdin = file
        self.io.attributes['stdin'] = self.io.stdin
        self.attributes['stdin'] = self.io.stdin

    def channel_out(self, file):
        self.io.stdout = file
        self.io.attributes['stdout'] = self.io.stdout
        self.attributes['stdout'] = self.io.stdout

    def channel_err(self, file):
        self.io.stderr = file
        self.io.attributes['stderr'] = self.io.stderr
        self.attributes['stderr'] = self.io.stderr

class System:
    def __init__(self):
        self.trace = SystemTrace()
        self.callstack_obj = IvyObj('CallStack', CallStack(), trace=self.trace)
        self.callstack = self.callstack_obj.object
        self.system = Record('<system>', RecordType.SYSTEM, 0)
        self.callstack.push(self.system)
        self.interpreter = Interpreter(callstack=self.callstack, trace=self.trace)
        self.io = IOModule(self.trace, self.interpreter)
        self.lexer_obj = IvyObj('Lexer', Lexer(trace=self.trace), self.interpreter, self.trace)
        self.lexer = self.lexer_obj.object
        self.objmachine = ObjectMachine(self.interpreter)
        self.parser = Parser(trace=self.trace, objmachine=self.objmachine)

        """ System Module """
        self.module = SystemModule(self)
        self.interpreter.add_env('System', self.module)

    """ LEXER METHODS """

    def tokenizefile(self, path):
        file = self.io.filefrompath(path)
        self.module.channel_in(file)
        return file, self.lexer.tokenizefile(file)

    """ RUN METHODS """

    def tokenized(self, path):
        try:
            for i in self.tokenizefile(path)[1]: print(i)
        except Error as e:
            print(e)

    def runfile(self, path):
        try:
            file, tokens = self.tokenizefile(path)
            self.module.channel_in(file)
            tree = self.parser.parse(file, tokens)
            res = self.interpreter.interpret(file, tree)
        except Error as e:
            print(e)

    """ REPL METHODS """

    def repl(self):
        self.stdfile = self.io.newstrfile('')
        self.module.channel_in(self.stdfile)

    def tokenizestring(self, code):
        file = self.io.newstrfile(code)
        return file, self.lexer.tokenizefile(file)

    def run_code(self, getin):
        try:
            self.stdfile.writeln(getin)
            file, tokens = self.tokenizestring(getin)
            tree = self.parser.parse(file, tokens)
            res = self.interpreter.interpret(file, tree)
            self.trace.clear()
        except Error as e:
            print(e)
