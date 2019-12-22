"""
***  IVY Language Interpreter
***  Universe Objects

"""
from src.error import *
from src.tokentype import TokenType
from src.callstack import RecordType, Record
from datetime import datetime

""" OBJECT TYPES """
TYPES = ['Integer', 'Float', 'String', 'Boolean', 'Collection', 'Function']

""" OPERATIONS ON OBJECTS """
BINOPMAP = {
    'op_add': lambda x,y: x+y,
    'op_sub': lambda x,y: x-y,
    'op_mult': lambda x,y: x*y,
    'op_div': lambda x,y: x/y,
    'op_mod': lambda x,y: x%y,
    'op_pow': lambda x,y: x**y,
    'op_lt': lambda x,y: x>y,
    'op_lte': lambda x,y: x >= y,
    'op_gt': lambda x,y: x < y,
    'op_gte': lambda x,y: x <= y,
    'op_eq': lambda x,y: x == y,
    'op_eq_not': lambda x,y: x != y,
    'op_not': lambda x: not x,
    'op_in': lambda x,y: x in y,
    'op_ideq': lambda x,y: type(x) == type(y),
    'op_ideq_not': lambda x,y: type(x) != type(y),
    'op_minus': lambda x: -x,
    'op_plus': lambda x: +x,
}

""" OBJECT MACHINE """
class ObjectMachine(object):
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.trace = interpreter.trace

    def new(self, obj, token=None):
        trace = self.trace
        if isinstance(obj, int):
            return Integer(obj, token, trace, self.interpreter)
        elif isinstance(obj, float):
            return Float(obj, token, trace, self.interpreter)
        elif isinstance(obj, str):
            return String(obj, token, trace, self.interpreter)
        elif isinstance(obj, bool):
            return Boolean(obj, token, trace, self.interpreter)
        elif isinstance(obj, list):
            return Collection(obj, token, trace, self.interpreter)
        elif callable(obj):
            return BuiltinMethod(obj, token, trace, self.interpreter)

    def fromtoken(self, token=None):
        trace = self.trace
        if token.type == TokenType.INTEGER_CONST:
            return Integer(token.value, token, trace, self.interpreter)
        elif token.type == TokenType.FLOAT_CONST:
            return Float(token.value, token, trace, self.interpreter)
        elif token.type == TokenType.STRING_CONST:
            return String(token.value, token, trace, self.interpreter)
        elif token.type == TokenType.TRUE:
            return Boolean(1, token, trace, self.interpreter)
        elif token.type == TokenType.FALSE:
            return Boolean(0, token, trace, self.interpreter)
        elif token.type == TokenType.NULL:
            return Null(token, trace, self.interpreter)

    def callable(self, params, code, token, name=None):
        return Function(name=name, params=params, code=code, token=token, trace=self.trace, interpreter=self.interpreter)

""" IVY OBJECT """
class IvyObject(object):

    """ INITIALIZE ATTRIBUTES """
    def __init__(self, name=None, type=None, token=None, trace=None, interpreter=None):

        """ Object Attributes """
        self.classname = self.__class__.__name__
        self.name = name if name != None else self.classname
        self.type = type if type != None else self.classname
        self.interpreter = interpreter
        self.token = token
        self.trace = trace

        """ Object Attributes """
        self.attributes = {
            'callable': self.newmethod(self.callable, [], 'callable'),
            'indexable': self.newmethod(self.indexable, [], 'indexable'),
            'istrue': self.newmethod(self.istrue, [], 'istrue'),
            'istype': self.newmethod(self.istype, ['type'], 'istype'),
            'repr': self.newmethod(self.representation, [], 'repr'),
            'newObject': self.newmethod(self.newobj, ['obj'], 'newObject'),
            'attrget': self.newmethod(self.attrget, ['attr'], 'attrget'),
            'attrset': self.newmethod(self.attrset, ['attr', 'value'], 'attrset'),
            'attrhas': self.newmethod(self.attrhas, ['attr'], 'attrhas'),
            'attrdel': self.newmethod(self.attrdel, ['attr'], 'attrdel'),
            'isnull': self.newmethod(self.isnull, [], 'isnull'),
            'printable': self.newmethod(self.printable, [], 'printable'),
        }

        """ Object Definition """
        self.objdef = AttributeObject({
            'name': self.name,
            'type': self.type,
            'class': self.getclass,
            'classname': self.classname,
            'instance': self.getinstance(),
            'callable': False,
            'indexable': False,
            'isnull': False,
            'istrue': True,
        },
            'inspect', self.token, self.trace, self.interpreter)
        self.attributes['inspect'] =  self.objdef
        self.attributes['access'] = AttributeObject(self.__dict__, 'access', self.token, self.trace, self.interpreter)

    """ OBJECT METHODS """
    def newobj(self, obj, token=None):
        token = self.token if token is None else token
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
    def newmethod(self, func, params, name):
        return BuiltinMethod(boundclass=self, func=func, params=params, name=name, token=self.token, trace=self.trace, interpreter=self.interpreter)
    def trynew(self, item, obj, type):
        try:
            n = obj(item)
            return n
        except ValueError:
            self.error('Casting ' + type(obj).__name__, etype=IvyValueError, mes='Cannot create a new ' + type + ' object')
    def getobjdef(self, name):
        return self.newobj(self.objdef[name])

    """ Binary Operations """
    def op_ideq(self, other): return Boolean(self.type == other.type)
    def op_ideq_not(self, other): return Boolean(self.type == other.type)
    def getop(self, attr, other): return self.dobinop(attr, other)

    """ OBJECT ATTRIBUTES """
    def attrget(self, att):
        if att == 'access':
            return AttributeObject(self.__dict__, 'access', self.token, self.trace, self.interpreter)
        if att in self.attributes:
            return self.newobj(self.attributes[att])
        return Boolean(False)
    def attrset(self, att, val):
        self.attributes[att] = self.newobj(val)
    def attrdel(self, att):
        if att in self.attributes:
            del self.attributes[att]
    def attrhas(self, att):
        if att in self.attributes:
            return Boolean(True)
        return Boolean(False)

    """ OBJECT INFORMATION """
    def getclass(self):
        return self.newobj(self.objdef['class'])
    def gettype(self):
        return self.newobj(self.objdef['type'])
    def getinstance(self):
        return self
    def callable(self):
        return self.objdef['callable']
    def indexable(self):
        return self.objdef['indexable']
    def istype(self, other):
        if isinstance(other, str):
            if self.objdef['type'].data == other:
                return Boolean(True)
        return Boolean(False)
    def istrue(self):
        return self.objdef['istrue']
    def isnull(self):
        return self.objdef['isnull']

    """ ERROR REPORTING """
    def error(self, type, etype, mes, token=None):
        token = self.token if token is None else token
        file = self.trace.peek()['file']
        self.trace.add(type=type, file=file, token=token)
        raise etype(mes, self.trace)
    def undefined(self, op, mes="", type1=None, type2=None):
        mes = "Operation '%s' not defined for types '%s' and '%s'" % (op, type1, type2) if mes == "" else mes
        self.error(type='Undefined Operation', etype=IvyUndefinedOperation, mes=mes)

    """ TO STRING METHODS """
    def newprintable(self, string):
        return self.newobj(string)
    def representation(self):
        return self.newprintable("<Ivy Object of type '%s' at 0x%08x>" % (self.type, id(self)))
    def printable(self):
        return self.newprintable(self.representation())
    def toprint(self):
        return self.representation().toprint()
    def __repr__(self):
        return self.toprint()
    def __str__(self):
        return self.toprint()

class AttributeObject(IvyObject):
    def __init__(self, objdef, name='attr', token=None, trace=None, interpreter=None):
        self.type = name
        self.name = name
        self.token = token
        self.trace = trace
        self.attributes = objdef
        self.interpreter = interpreter

    def __getitem__(self, att):
        return self.newobj(self.attributes[att])

    def __setitem__(self, att, val):
        self.attributes[att] = val

    def __delitem__(self, att):
        self.attributes.pop()

    def attrget(self, att):
        if att in self.attributes:
            return self.newobj(self.attributes[att])
        return Boolean(False)

    def attrset(self, att, val):
        self.attributes[att] = val

    def attrdel(self, att, val):
        self.attributes.pop(att)

    def representation(self):
        return self.newprintable("<Attribute Object '%s' at 0x%08x>" % (self.type, id(self)))

class Null(IvyObject):
    def __init__(self, token=None, trace=None, interpreter=None):
        super().__init__(type='Null', token=token, trace=trace, interpreter=interpreter)
        self.objdef['istrue'] = False
        self.objdef['isnull'] = True

    def dobinop(self, op, other, searchm=False):
        if 'op_'+op in BINOPMAP.keys():
            self.undefined(op, mes="No binary operation for object of type 'Null' is allowed!")
        self.undefined(op, mes="No operation for object of type 'Null' is allowed!")

    def printable(self):
        return self.newprintable('null')

class DataObject(IvyObject):
    def __init__(self, type=None, data=None, token=None, trace=None, interpreter=None):
        self.data = data
        super().__init__(type=type, token=token, trace=trace, interpreter=interpreter)
        self.objdef['data'] = data
        self.objdef['istrue'] = True
        self.objdef['isnull'] = False

    def op_lt(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data > other.data)
        elif isinstance(other, Float):
            return Boolean(self.data > other.data)
        self.undefined('lt', self.type, other.type)

    def op_lte(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data >= other.data)
        elif isinstance(other, Float):
            return Boolean(self.data >= other.data)
        self.undefined('lte', self.type, other.type)

    def op_gt(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data < other.data)
        elif isinstance(other, Float):
            return Boolean(self.data < other.data)
        self.undefined('gt', self.type, other.type)

    def op_gte(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data <= other.data)
        elif isinstance(other, Float):
            return Boolean(self.data <= other.data)
        self.undefined('gte', self.type, other.type)

    def dobinop(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Integer):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Float):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, String):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        self.undefined(op, mes="Binary operation not defined for types " + "'" + self.type + "' and '" + other.type + "'.")
        return False

    def op_eq(self, other):
        if getattr(other, 'data', False) != False:
            return Boolean(self.data == other.data)
        return Boolean(self.data == other)

    def op_eq_not(self, other):
        if getattr(other, 'data', False) != False:
            return Boolean(self.data != other.data)
        return Boolean(self.data != other)

    def toprint(self):
        return str(self.data)

    def printable(self):
        return self.newprintable(self.toprint())

class Integer(DataObject):
    def __init__(self, data, token=None, trace=None, interpreter=None):
        super().__init__(type='Integer', data=self.trynew(data, int, 'Integer'), token=token, trace=trace, interpreter=interpreter)
        self.objdef['indexable'] = True
        self.objdef['istrue'] = True
        self.objdef['isnull'] = False
        self.objdef['callable'] = False

    def getitem(self, ref):
        if isinstance(ref, Integer):
            return String(str(self.data)[ref.data])
        self.error(type='Calling Index',etype=IvyIndexError, mes="Integer index calls must be of type 'Integer'")

    def op_plus(self):
        return Integer(self.data)

    def op_minus(self):
        return Integer(-self.data)

    def op_div(self, other):
        if other.data == 0:
            self.error(type='Binary Division', etype=IvyZeroDivisionError, mes='Cannot divide by zero')
        if isinstance(other, Integer):
            return self.newobj(self.data / other.data)
        elif isinstance(other, Float):
            return self.newobj(self.data / other.data)
        self.undefined(op, self.type, other.type)

    def op_not(self):
        self.undefined()

    def istrue(self):
        return Boolean(True) if abs(self.data) > 0 else Boolean(False)

    def dobinop(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(BINOPMAP['op_' + op](self.data, other.data))
        elif isinstance(other, Integer):
            return self.op_result(BINOPMAP['op_' + op](self.data, other.data))
        self.undefined(op, mes="Binary operation not defined for types " + "'" + self.type + "' and '" + other.type + "'.")
        return False

    def op_result(self, data):
        return Integer(data)

class Float(DataObject):
    def __init__(self, data, token=None, trace=None, interpreter=None):
        super().__init__(type='Float', data=self.trynew(data, float, 'Float'), token=token, trace=trace, interpreter=interpreter)
        self.objdef['indexable'] = True

    def getitem(self, ref):
        if isinstance(ref, Integer):
            return String(str(self.data)[ref.data])
        self.error(type='Calling Index',etype=IvyIndexError, mes="Float index calls must be of type 'Integer'")

    def op_plus(self):
        return Integer(self.data)

    def op_minus(self):
        return Integer(-self.data)

    def op_not(self):
        self.undefined()

    def istrue(self, other):
        return Boolean(True) if abs(self.data) > 0 else Boolean(False)

    def dobinop(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(BINOPMAP['op_' + op](self.data, other.data))
        elif isinstance(other, Integer):
            return self.op_result(BINOPMAP['op_' + op](self.data, other.data))
        elif isinstance(other, Float):
            return self.op_result(BINOPMAP['op_' + op](self.data, other.data))
        self.undefined(op, mes="Binary operation not defined for types " + "'" + self.type + "' and '" + other.type + "'.")
        return False

    def op_result(self, data):
        return Float(data)

class String(DataObject):
    def __init__(self, data, token=None, trace=None, interpreter=None):
        super().__init__(type='String', data=self.trynew(data, str, 'String'), token=token, trace=trace, interpreter=interpreter)
        self.len = len(self.data)
        self.objdef['length'] = len(self.data)
        self.attributes['length'] = self.newmethod(self.length, [], 'length')
        self.objdef['indexable'] = True

    def op_in(self, other):
        return Boolean(other.data in self.data)

    def iterate(self):
        return Iterator(self.data, token=self.token, trace=self.trace, interpreter=self.interpreter)

    def istrue(self):
        return len(self.data)

    def length(self):
        return Integer(self.len)

    def toprint(self):
        return str(self.data)

    def getitem(self, ref):
        if isinstance(ref, Integer):
            return String(self.data[ref.data])
        self.error(type='Calling Index',etype=IvyIndexError, mes="String index calls must be of type 'Integer'")

    def op_plus(self):
        self.undefined('plus')

    def op_minus(self):
        self.undefined('minus')

    def op_not(self):
        self.undefined('not')

    def istrue(self):
        return Boolean(True) if len(self.data) > 0 else Boolean(False)

    def op_mult(self, other):
        if isinstance(other, Boolean):
            return String(self.data if other.data == 1 else "")
        if isinstance(other, Integer):
            return ''.join([self.data for i in range(other.data)])
        elif isinstance(other, Float):
            self.undefined()
        elif isinstance(other, String):
            self.undefined()

    def op_lt(self, other):
        if isinstance(other, Integer):
            return Boolean(len(self.data) > other.data)
        elif isinstance(other, String):
            return Boolean(len(self.data) > len(other.data))
        self.undefined()

    def op_lte(self, other):
        if isinstance(other, Integer):
            return Boolean(len(self.data) >= other.data)
        elif isinstance(other, String):
            return Boolean(len(self.data) >= len(other.data))
        self.undefined()

    def op_gt(self, other):
        if isinstance(other, Integer):
            return Boolean(len(self.data) < other.data)
        elif isinstance(other, String):
            return Boolean(len(self.data) < len(other.data))
        self.undefined()

    def op_gte(self, other):
        if isinstance(other, Integer):
            return Boolean(len(self.data) <= other.data)
        elif isinstance(other, String):
            return Boolean(len(self.data) <= len(other.data))
        self.undefined()

    def op_mod(self, other):
        self.undefined()

    def dobinop(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Integer):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Float):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, String):
            return self.op_result(BINOPMAP['op_' + op](str(self.data), str(other.data)))
        self.undefined(op, mes="Binary operation not defined for types " + "'" + self.type + "' and '" + other.type + "'.")
        return False

    def op_result(self, data):
        return String(data)

class Boolean(DataObject):
    def __init__(self, booldata, token=None, trace=None, interpreter=None):
        if booldata:
            booldata = 1
            boolean = True
        else:
            booldata = 0
            boolean = False
        super().__init__(type='Boolean', data=booldata, token=token, trace=trace, interpreter=interpreter)
        self.objdef['istrue'] = boolean

    def op_plus(self):
        self.undefined()

    def op_minus(self):
        self.undefined()

    def op_not(self):
        return Boolean(not bool(self.data))

    def op_and(self, other):
        return Boolean(bool(self.data) and bool(other.data))

    def op_or(self, other):
        return Boolean(bool(self.data) or bool(other.data))

    def dobinop(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(BINOPMAP['op_' + op](self.data, other.data))
        self.undefined(op, mes="Binary operation not defined for types " + "'" + self.type + "' and '" + other.type + "'.")
        return False

    def op_result(self, data):
        return Boolean(data)

    def toprint(self):
        if self.data:
            return 'true'
        return 'false'

    def __bool__(self):
        return self.data > 0

class Collection(DataObject):
    def __init__(self, data, token=None, trace=None, interpreter=None):
        super().__init__(type='Collection', data=data, token=token, interpreter=interpreter)
        self.len = len(self.data)
        self.objdef['collection'] = data
        self.objdef['length'] = len(data)
        self.attributes['length'] = self.newmethod(self.length, [], 'length')
        self.objdef['indexable'] = True

    def op_in(self, other):
        return Boolean(other.data in self.data)

    def iterate(self):
        return Iterator(self.data, token=self.token, trace=self.trace, interpreter=self.interpreter)

    def getitem(self, ref):
        if isinstance(ref, Integer):
            return self.objdef['obj_coll'][ref.data]
        self.error(type='Calling Index',etype=IvyIndexError, mes="Collection index calls must be of type 'Integer'")

    def additem(self, val):
        self.objdef['collection'].append(val)

    def delitem(self, val):
        self.objdef['collection'].remove(val)

    def istrue(self):
        return Boolean(self.len > 0)

    def length(self):
        return Integer(self.len)

    def printable(self):
        pt = '['
        for n,i in enumerate(self.data):
            pt += i.toprint()
            if n!=len(self.data)-1: pt += ', '
        return self.newprintable(pt + ']')

class Iterator(IvyObject):
    def __init__(self, iterable, itertype, token=None, trace=None, interpreter=None):
        super().__init__(name='<Iterator>', type='Iterator', token=token, trace=trace, interpreter=interpreter)
        self.iterable = iterable
        self.itertype = itertype
        self.length = len(iterable)
        self.counter = 0
        self.current = None
        self.attributes.update({
            'next': self.newmethod(self.next, [], 'next'),
            'all': self.newmethod(self.getall, [], 'all'),
        })
        self.objdef['indexable'] = True

    def next(self):
        if self.counter == self.length:
            self.error(type='Iteration', etype=IvyIterationError, mes='End of iteration')
        self.current = self.iterable[self.counter]
        self.counter += 1
        return self.newobj(self.current)

    def getall(self):
        return self.newobj(self.iterable)

    def representation(self):
        return self.newprintable("<Iterator of '%s' at 0x%08x>" % (self.type, id(self)))

class Function(IvyObject):
    def __init__(self, params, code, name='<Function>', token=None, trace=None, interpreter=None):
        super().__init__(type='Function', token=token, trace=trace, interpreter=interpreter)
        self.interpreter = interpreter
        self.anonymous = True
        self.reference = name
        self.type = 'Function'
        self.name = name
        self.params = params if params else []
        self.realparams = params
        if params:
            try:
                self.realparams = [i.value for i in params] if params != [] else []
            except AttributeError:
                self.realparams = params
        self.block = code
        self.native = False
        self.objdef['reference'] = self.reference
        self.objdef['name'] = self.name
        self.objdef['params'] = self.realparams
        self.objdef['block'] = self.block
        self.objdef['callable'] = True
        self.attributes['call'] = self.newmethod(self.callmet, self.params, 'call')

    def process_param(self, param):
        if isinstance(param, String):
            return '"' + str(param.data) + '"'
        elif isinstance(param, Boolean):
            return str(param.__bool__())
        return str(param.data)

    def checkargs(self, args, paramtoken):
        if len(self.params) != len(args):
            self.error(type='Checking Arguments', mes="'{}' takes {} arguments, but {} were given".format(self.name, len(self.params), len(args)),
                       token=paramtoken, etype=IvyCallError)

    def callmet(self, *args):
        class Call:
            def __init__(self): pass
        node = Call()
        node.variable = self.token
        node.list_expr = []
        for a in args:
            node.list_expr.append(self.newobj(a))
        return self.call(node)

    def call(self, node):
        args = node.list_expr
        cur_record = self.interpreter.callstack.peek()
        cur_depth = cur_record.depth
        record = self.interpreter.callstack.copy(Record(self.name, RecordType.FUNCTION, cur_depth+1))
        paramtoken = self.interpreter.visit(args[0]).token if len(args) != 0 else node.variable
        self.checkargs(args, paramtoken)
        eargs = []
        for n, i in enumerate(self.params):
            val = self.interpreter.visit(args[n])
            if not self.native:
                record[i.value] = val
            eargs.append(val)
        self.interpreter.callstack.push(record)
        if self.native:
            call = self.do(eargs, args, self.interpreter, node)
        else:
            call = self.interpreter.visit(self.block)
        self.interpreter.callstack.pop()
        if not call:
            obj = self.newobj('null', node.variable)
            return obj
        elif not isinstance(call, IvyObject):
            return self.newobj(call)
        return call

    def representation(self):
        if self.anonymous:
            return self.newprintable("<Anonymous Function at 0x%08x>" % (id(self)))
        else:
            return self.newprintable("<Function (reference: %s) at 0x%08x>" % (self.reference, id(self)))

class Method(IvyObject):
    def __init__(self, boundclass=None, params=[], code=None, name='<Method>', token=None, trace=None, interpreter=None):
        self.interpreter = interpreter
        self.bound = boundclass
        self.token = token
        self.trace = trace
        self.classname = self.__class__.__name__
        self.anonymous = False
        self.reference = name
        self.type = 'Method'
        self.name = name
        self.params = params if params else []
        self.realparams = params
        if params:
            try:
                self.realparams = [i.value for i in params] if params != [] else []
            except AttributeError:
                self.realparams = params
        self.block = code
        self.native = False
        self.objdef = AttributeObject({
            'name': self.name,
            'type': self.type,
            'self': self.getinstance(),
            'class': self.getclass,
            'classname': self.classname,
        }, 'inspect', self.token, self.trace, self.interpreter)
        self.attributes = {
            'inspect': self.objdef
        }
        self.objdef['reference'] = self.reference
        self.objdef['params'] = self.realparams
        self.objdef['block'] = self.block
        self.objdef['bound'] = self.bound
        self.objdef['callable'] = True

    def process_param(self, param):
        if isinstance(param, String):
            return '"' + str(param.data) + '"'
        elif isinstance(param, Boolean):
            return str(param.__bool__())
        return str(param.data)

    def checkargs(self, args, paramtoken):
        if len(self.params) != len(args):
            self.error(type='Checking Arguments', mes="'{}' takes {} arguments, but {} were given".format(self.name, len(self.params), len(args)),
                       token=paramtoken, etype=IvyCallError)

    def call(self, node):
        args = node.list_expr
        cur_record = self.interpreter.callstack.peek()
        cur_depth = cur_record.depth
        record = self.interpreter.callstack.copy(Record(self.name, RecordType.FUNCTION, cur_depth+1))
        paramtoken = self.interpreter.visit(args[0]).token if len(args) != 0 else node.variable
        print(paramtoken)
        self.checkargs(args, paramtoken)
        eargs = []
        for n, i in enumerate(self.params):
            val = self.interpreter.visit(args[n])
            if not self.native:
                record[i.value] = val
            eargs.append(val)
        self.interpreter.callstack.push(record)
        if self.native:
            call = self.do(eargs, args, self.interpreter, node)
        else:
            call = self.interpreter.visit(self.block)
        self.interpreter.callstack.pop()
        if not call:
            obj = self.newobj('null', node.variable)
            return obj
        elif not isinstance(call, IvyObject):
            return self.newobj(call)
        return call

    def representation(self):
        return self.newprintable("<Method bound to '%s' (reference: '%s') at 0x%08x>" % (self.bound.__class__.__name__, self.reference, id(self)))

class BuiltinMethod(Method):
    def __init__(self, boundclass=None, func=None, params=[], name='<BuiltinMethod>', token=None, trace=None, interpreter=None):
        super().__init__(boundclass=boundclass, params=params, code=None, name='<Method>', token=token, trace=trace, interpreter=interpreter)
        self.type='BuiltinMethod'
        self.name = name
        self.func = func
        self.native = True
        self.objdef['reference'] = name

    def do(self, args, params, interpreter, node):
        func = self.func
        params = ','.join([self.process_param(i) for i in args]) if args != [] else ''
        return eval('func(%s)' % (params))
