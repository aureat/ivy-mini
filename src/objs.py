"""
***  IVY Language Interpreter
***  Universe Objects

* Wrapper
* Instance
* Struct
* Object
* Function
* FunctionWrapper

Some features for brain storm:
- interpreter uses the general purpose getattribute method
    to do binary operation, index call and so on.
- however the attribute access syntax uses the struct's own getattribute method

TODO:
* repr should point to the representation method implemented
  or otherwise (via getattr)
* implement a dictionary only attribute retrieval, so that there are no
  need for python class attributes (self.*)
* implement a prototype system, changing super would result in inheritance change

"""

from src.env import Env
from src.ast import ReturnValue
from src.error import *
from src.tokentype import TokenType
from inspect import signature

"""
*** TOOLBOX
"""
def bound_method(inst, method):
    def bound(*args):
        return method(inst, *args)
    return bound

def bind(instance, func):
    bound_method = func.__get__(instance, instance.__class__)
    as_name = func.__name__
    setattr(instance, as_name, bound_method)
    return bound_method

def iscallable(obj):
    return isinstance(obj, Function) or callable(obj) or obj.hasattr('call')

"""
*** OBJECT MODEL
"""
class Wrapper(dict):
    def __init__(self, struct):
        self.setstruct(struct)
        self.init_builtins()

    # Add attributes
    def addattr(self, name):
        if hasattr(self.struct, name):
            self[name] = getattr(self.struct, name)
        else:
            self[name] = getattr(self, name)

    # Initialization
    def init_builtins(self):
        self['list'] = self.list
        self['getattr'] = self.getattr
        self['setattr'] = self.setattr
        self['hasattr'] = self.hasattr
        self['delattr'] = self.delattr
        self.addattr('repr')
        self.addattr('string')

    def initobj(self, name):
        self.setname(name)
        self.settype(name)
        self.setstruct(self)
        self.setparent(None)

    def setstruct(self, struct):
        self.struct = struct
        self['structure'] = struct

    def setparent(self, parent):
        self.parent = parent
        self['model'] = parent

    def setname(self, name):
        self.name = name
        self['name'] = name

    def settype(self, name):
        self.type = name
        self['type'] = type

    def setinst(self):
        self.type = self.struct.name
        self['type'] = self.struct['name']

    # Object Methods
    def list(self):
        return set(list(self.keys()) + self.struct.getlist())

    def getattr(self, name):
        if name in self.keys():
            return self[name]
        try:
            attribute = self.struct.getinstattr(name)
            return attribute
        except IvyAttributeError:
            return None

    def setattr(self, name, value):
        self[name] = value

    def hasattr(self, name):
        if name in self.keys():
            return True
        try:
            attribute = self.struct.getinstattr(name)
            return True
        except IvyAttributeError:
            return False

    def delattr(self, name):
        if name in self.keys():
            del self[name]
            return
        self.struct.delattr(name)

    def repr(self):
        return "<Object '%s'>" % (self.struct.name)

    def string(self):
        return self.repr()

    def __repr__(self):
        return self.repr()

    def __str__(self):
        return self.string()

class Instance(Wrapper):
    def __init__(self, struct):
        Wrapper.__init__(self, struct)
        self.test = "this is an instance"
        self.setinst()

    def construct(self, *args):
        # constructor = self.struct.constructor
        # constructor(self, *args)
        constructor = self.getattr("construct")
        if constructor:
            constructor.call(*args)

def isa(obj, type):
    return obj.type == type

class Struct(Wrapper):
    def __init__(self):
        # Initialize Structure
        super().__init__(self)
        self.initobj('<Structure>')
        # Env and Block
        self.env = None
        self.block = None
        self.attributes = {}
        # Attributes
        self['inheritance'] = self.inheritance
        self['arity'] = self.arity
        self['repr'] = self.struct_repr
        self['string'] = self.struct_string

    def initenv(self, parent):
        self.env = Env(self.name, parent)
        self.env.define('obj', self)

    def setparentenv(self):
        self.initenv(self.parent.env)

    def list(self):
        return set(self.getlist())

    def getlist(self):
        if self.parent is None: return list(self.keys())
        else:
            return list(self.keys()) + self.parent.getlist()

    def inheritance(self):
        if self.parent is None: return [self]
        else:
            return [self] + self.parent.inheritance()

    def isparent(self, struct):
        return struct in self.inheritance()

    def getinstattr(self, key):
        if key in self.keys():
            return self[key]
        elif self.parent:
            return self.parent.getattr(key)
        raise IvyAttributeError()

    def getattr(self, key):
        if key in self.keys():
            return self[key]
        elif key in self.attributes:
            return self.attributes[key]
        elif self.parent:
            try:
                return self.parent.getattr(key)
            except IvyAttributeError:
                pass
        raise IvyAttributeError()

    def setattr(self, key, value):
        self[key] = value

    def hasattr(self, key):
        if key in self.keys():
            return True
        elif key in self.attributes:
            return True
        elif self.parent:
            try:
                return self.parent.hasattr(key)
            except IvyAttributeError:
                pass
        raise IvyAttributeError()

    def delattr(self, key):
        if key in self.keys():
            del self[key]
        elif key in self.attributes:
            del self.attributes[key]

    def setstatic(self, key, value):
        self.attributes[key] = value

    def arity(self):
        initializer = self.getattr("construct")
        if initializer:
            return initializer.args
        return 0

    def struct_repr(self):
        return "<Structure '%s'>" % (self.name)

    def struct_string(self):
        return self.struct_repr()

    def __repr__(self):
        return self.struct_repr()

    def __str__(self):
        return self.struct_string()

"""
*** THE OBJECT STRUCTURE
"""
class Object(Struct):
    def __init__(self):
        super().__init__()
        self.initobj('Object')
        self.obj_builtins()
        self['construct'] = FunctionWrapper(self.constructor)

    def constructor(self):
        pass

    def obj_builtins(self):
        self['istrue'] = self.istrue
        self['isnull'] = self.isnull
        self['callable'] = self.callable
        self['indexable'] = self.indexable
        self['_ideq'] = self.ideq
        self['_notideq'] = self.notideq

    def hasattr(self, name):
        return name in self.keys()

    def delattr(self, name):
        del self[name]

    def istrue(self):
        return True

    def isnull(self):
        return False

    def callable(self):
        return self.hasattr('call')

    def indexable(self):
        return self.hasattr('getitem')

    def ideq(self, other):
        return self.getattr('type') == other.getattr('type')

    def notideq(self, other):
        return not self.ideq(other)

    def string(self):
        return "<Object '%s'>" % (self.struct.name)

    def repr(self):
        return "<Object '%s'>" % (self.struct.name)

class DataObject(Object):
    def __init__(self):
        super().__init__()
        self['value'] = None
        self['_eq'] = self.eq
        self['_noteq'] = self.noteq

    def dataobj(self, name):
        self.initobj(name)
        self.setparent(Object)

    def eq(self, other):
        return self.value == other.value

    def noteq(self, other):
        return not self.op_eq(other)

# class Integer(DataObject):
#     def __init__(self):
#         super().__init__()
#         self.value = None
#         self.dataobj('Integer')
#         self['construct'] = Integer.constructor
#         self['_add'] = Integer.add
#
#     @staticmethod
#     def add(left, other):
#         return left.value + other.value
#
#     @staticmethod
#     def constructor(left, value):
#         left.value = value

class Function(Wrapper):
    def __init__(self, params, code, name, token, ltoken, trace):
        super().__init__(self)
        self.initobj('Function')
        self.setname(name if name is not None else '<Function>')
        self.setparent(Object)
        # Function Defaults
        self.params = params
        self.anonymous = False
        self.reference = self.name
        self.block = code
        self.closure = None
        self.isinit = False
        self.token = token
        self.ltoken = ltoken
        self.trace = trace
        self.args = len(self.params) if self.params else 0
        self.interp = None
        # Structure Attributes
        self['args'] = self.args
        self['call'] = self.call
        self['isconstructor'] = False
        self['string'] = self.string

    def call(self, *args):
        env = Env(name=self.name, parent=self.closure)
        params = self.params
        for param, arg in zip(params, args):
            env.define(param.value, arg)
        try:
            exc = self.interp.execute_block(self.block, env)
        except ReturnValue as rv:
            return self.interp.visit(rv.value)
        return None

    def string(self):
        return self.repr()

    def repr(self):
        return "<Function '%s' bound to '%s'>" % (self.name, self.closure.name)

class FunctionWrapper(Function):
    def __init__(self, boundfunc):
        super().__init__(None, None, boundfunc.__name__, None, None, None)
        sig = signature(boundfunc)
        self.params = None
        self.args = len(sig.parameters)
        self.isinit = False
        self.anonymous = False
        self.reference = self.name
        self.block = boundfunc
        self.closure = None
        self.token = None
        self.ltoken = None
        self.trace = None
        self.parent = Object
        self['name'] = self.name
        self['args'] = self.args
        self['isconstructor'] = self.isinit
        self['call'] = self.call

    def repr(self):
        return "<Function wrapper '%s'>" % (self.name)

    def call(self, *args):
        return self.block(*args)
