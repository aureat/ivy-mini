"""
***  IVY Language Interpreter
***  Environment
"""

from src.error import IvyNameError, IvySystemError, IvyRuntimeError

MAX_SCOPE_DEPTH = 100

class Env(dict):
    def __init__(self, name='<local>', parent=None, params=[], args=[], interp=None):
        self.update(zip(params, args))
        self.parent = parent
        self.isglobal = (parent is None)
        self.interp = interp
        self.depth = 0
        self.name = name if name is not None else '<local:' + str(self.depth) + '>'
        if self.isglobal:
            self.name = '<global>'
            self.depth = 0
        else:
            self.depth = self.parent.depth + 1
        if self.depth > MAX_SCOPE_DEPTH:
            self.error(mes='Maximum scope depth exceeded (maxdepth: %s)' % (MAX_SCOPE_DEPTH),
                             etype=IvySystemError)

    def error(self, mes, etype=IvyRuntimeError):
        self.trace.add(type='Runtime', token=None, framename=self.env.name, file=self.interp.file)
        raise etype(mes, self.interp.trace)

    def getenv(self, name):
        if name in self:
            return self
        elif not self.isglobal:
            return self.parent.getenv(name)
        raise IvyNameError('Name not found', self.interp.trace)

    def ancestor(self, distance):
        result = self
        for i in range(distance):
            result = result.parent
        return result

    def define(self, name, value):
        self[name] = value

    def assign(self, name, value):
        self.getenv(name)[name] = value

    def assign_at(self, distance, name, value):
        self.ancestor(distance)[name] = value

    def lookup(self, name):
        return self.getenv(name)[name]

    def look_at(self, distance, name):
        return self.ancestor(distance)[name]

    def remove_at(self, distance, name):
        return self.ancestor(distance).pop(name)

    def makechild(self, name=None, params=[], args=[]):
        return Env(name=name, params=params, args=args, parent=self, interp=self.interp)
