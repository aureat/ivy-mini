"""
*** FILES AND PACKAGES
"""
import sys
from src.error import IvyIOError
from src.module import Module
from src.objects import *

class IOModule(Module):
    def __init__(self, trace, interpreter):
        super().__init__('IOModule', trace, interpreter)
        self.trace = trace
        self.stdin = sys.stdin
        self.stdout = sys.stdout
        self.stderr = sys.stderr
        self.attributes.update({
            'stdin': self.stdin,
            'stdout': self.stdout,
            'stderr': self.stderr,
        })

    def error(self, mes):
        raise IvyIOError(mes, self.trace)

    def filefrompath(self, path):
        tryfullpath = os.path.join(os.path.dirname(os.path.abspath(__file__)), path)
        fullpath = path
        if os.path.exists(tryfullpath):
            fullpath = tryfullpath
        try:
            filepath = fullpath
            filename = fullpath.split('/')[-1]
            return IOWrapper(filename, filepath, trace=self.trace, interpreter=self.interpreter)
        except IOError:
            self.error('Cannot find file from specified path')

    def newstrfile(self, code):
        path = '<String>'
        iowrap = IOWrapper(path, path, read=False, trace=self.trace, interpreter=self.interpreter)
        iowrap.wrapper = path
        iowrap.contents = str(code)
        return iowrap

    def __repr__(self):
        return '<System.IO>'

    __str__ = __repr__

class IOWrapper(IvyObject):
    def __init__(self, name, path, read=True, trace=None, interpreter=None):
        super().__init__(name=name, type='IOWrapper', token=None, trace=trace, interpreter=interpreter)
        self.contents = ''
        if read:
            with open(path, 'r') as file:
                self.wrapper = file
                self.contents = file.read()
        self.name = name
        self.path = path
        self.line_counter = 0
        self.attributes.update({
            'getline': self.newmethod(self.getline_ivy, ['ind'], 'getline'),
            'writeln': self.newmethod(self.writeln, ['line'], 'writeln'),
            'readline': self.newmethod(self.readline_ivy, [], 'readline'),
        })

    def __repr__(self):
        return f'<System.IO.IOWrapper name={self.name} path={self.path}>'

    __str__ = __repr__

    def split(self):
        return self.contents.split('\n')

    def getline(self, ind):
        return self.split()[ind]

    def getline_ivy(self, ind):
        return self.newobj(self.getline(ind))

    def readline(self):
        content = self.split()[self.line_counter]
        self.line_counter += 1
        return content

    def readline_ivy(self):
        return self.newobj(self.getline())

    def read(self):
        return self.contents

    def writeln(self, line):
        self.contents += line + '\n'

    def write(self, line):
        if self.path:
            with open(path, 'w') as file:
                self.wrapper = file
                file.write(line)
        return Boolean(False)
