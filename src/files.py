"""
*** IVY Langauge Interpreter
***
"""

import os
from src.error import IvyIOError

class File:
    def __init__(self):
        self.name = None
        self.path = None
        self.contents = None

    def split(self):
        return self.contents.split('\n')

    def getline(self, ind):
        return self.split()[ind]

    def __str__(self):
        return f'<System.IO.File name={self.name} path={self.path}>'

    __repr__ = __str__

class IOWrapper:
    def __init__(self, trace):
        self.trace = trace
        self.file = None
        self.contents = None

    def error(self, mes):
        raise IvyIOError(mes, self.trace)

    def frompath(self, path):
        tryfullpath = os.path.join(os.path.dirname(os.path.abspath(__file__)), path)
        fullpath = path
        if os.path.exists(tryfullpath):
            fullpath = tryfullpath
        try:
            self.path = fullpath
            self.name = fullpath.split('/')[-1]
            self.contents = self.read()
        except IOError:
            self.error('Cannot find file from specified path')

    def fromstr(self, str):
        self.path = self.name = '<String>'
        self.contents = str

    def fromfile(self, file):
        self.name = file.name
        self.path = file.name
        self.file = file

    def newfile(self):
        file = File()
        file.name = self.name
        file.path = self.path
        if not self.contents:
            file.contents = self.read()
        else:
            file.contents = self.contents
        return file

    def write(self, line):
        if not self.file and os.path.exists(self.path):
                self.file = open(self.path, 'w+')
        self.file.write(line)

    def read(self):
        if not self.file and os.path.exists(self.path):
            self.file = open(self.path, 'r')
        return self.file.read()

    def readline(self):
        if not self.file and os.path.exists(self.path):
            self.file = open(self.path, 'r')
        return self.file.readline()

    def close(self):
        self.file.close()
        self.file = None

    def __str__(self):
        return f'<System.IO.IOWrapper name={self.name} path={self.path}>'

    __repr__ = __str__
