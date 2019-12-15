"""
*** SYSTEM OBJECTS

(1) Object Priority: Collection > String > Float > Integer > Boolean

"""

from error import *
from tokentype import TokenType
from callstack import RecordType, Record
from datetime import datetime

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

""" OBJECT CLASSES """

class ObjectMachine(object):
    def __init__(self, trace):
        self.trace = trace

    def new(self, obj, token=None):
        trace = self.trace
        if isinstance(obj, int):
            return Integer(obj, token, trace)
        elif isinstance(obj, float):
            return Float(obj, token, trace)
        elif isinstance(obj, str):
            return String(obj, token, trace)
        elif isinstance(obj, bool):
            return Boolean(obj, token, trace)
        elif isinstance(obj, list):
            return Collection(obj, token, trace)
        elif callable(obj):
            return PFunction(obj, token, trace)

    def fromtoken(self, token=None):
        trace = self.trace
        if token.type == TokenType.INTEGER_CONST:
            return Integer(token.value, token, trace)
        elif token.type == TokenType.FLOAT_CONST:
            return Float(token.value, token, trace)
        elif token.type == TokenType.STRING_CONST:
            return String(token.value, token, trace)
        elif token.type == TokenType.TRUE:
            return Boolean(1, token, trace)
        elif token.type == TokenType.FALSE:
            return Boolean(0, token, trace)
        elif token.type == TokenType.NULL:
            return Null(token, trace)

class IvyObject(object):

    """
        ABSTRACT CLASS for Ivy Objects
        * Objdef Attribute: object definition of an ivy object
        * Access Method: gives access to all attributes of an ivy object
        * Inspect Method: gives access to all object definition of an ivy object
        * Attributes attr.: object attributes

    """

    """ INITIALIZE IVY OBJECT """
    def __init__(self, name=None, type=None, token=None, trace=None):

        """ Object Attributes """
        self.classname = self.__class__.__name__
        self.name = name if name != None else self.classname
        self.type = type if type != None else self.classname
        self.token = token
        self.trace = trace

        """ Object Attributes """
        self.attributes = {
            'istrue': BuiltinMethod(boundclass=self, func=self.istrue, params=[], name='istrue', token=self.token, trace=self.trace)
        }

        """ Initial Object Definition """
        self.objdef = AttributeObject({
            'name': self.name,
            'type': self.type,
            'self': self.getobj(),
            'class': self.getclass,
            'classname': self.classname,
            'attributes': self.attributes,
        }, 'inspect', self.token, self.trace)
        self.attributes['inspect'] =  self.objdef

    def newobj(self, obj, token=None):
        token = self.token if token is None else token
        if isinstance(obj, int):
            return Integer(obj, token, self.trace)
        elif isinstance(obj, float):
            return Float(obj, token, self.trace)
        elif isinstance(obj, str):
            return String(obj, token, self.trace)
        elif isinstance(obj, bool):
            return Boolean(obj, token, self.trace)
        elif obj == 'null':
            return Null(token, self.trace)
        elif isinstance(obj, list):
            return Collection(obj, token, self.trace)
        elif callable(obj):
            return PFunction(obj, token, self.trace)
        return obj

    def trynew(self, item, obj, type):
        try:
            n = obj(item)
            return n
        except ValueError:
            self.error('Casting ' + type(obj).__name__, etype=IvyValueError, mes='Cannot create a new ' + type + ' object')

    "ALL PROPERTIES"
    if True:
        pass
        """
            Ivy Object Binary Operations
            * op_add
            * op_sub
            * op_mult
            * op_div
            * op_mod
            * op_pow
            * op_lt
            * op_lte
            * op_gt
            * op_gte
            * op_eq
            * op_eq_not
            * op_not
            * op_in
            * op_ideq
            * op_ideq_not
            * op_minus
            * op_plus

            General Ivy Object Methods
            * error
            * undefined
            * istrue
            * isfalse
            * getprop
            * setprop
            * delprop
            * attrget
            * attrset
            * attrdel
            * getmethod
            * istype
            * kill : delete reference, leave for python garbage collection
            * printable
            * inspect
            * access

            Ivy Object Type-specific Methods
            * next
            * iterate
            * isnull
            * length
            * getitem
            * setitem
            * delitem

        """

    """ Method Execution """
    def domethod(self): pass

    """ Binary Operations """
    def op_ideq(self, other):
        return Boolean(self.type == other.type)

    def op_ideq_not(self, other):
        return Boolean(self.type == other.type)

    def getop(self, attr, other):
        return self.dobinop(attr, other)

    """ Python Attributes """
    def getprop(self, att):
        return getattr(self, att, False)

    def setprop(self, att, val):
        setattr(self, att, val)

    def delprop(self, att):
        delattr(self, att)

    def attrget(self, att):
        if att in self.attributes:
            return self.newobj(self.attributes[att])
        return False

    def attrset(self, att, val):
        self.attributes[att] = val

    def attrdel(self, att):
        del self.attributes[att]

    def getmethod(self, att):
        if att in self.objdef:
            if callable(self.objdef[att]):
                return self.objdef[att]
        getmet = self.getprop(att)
        if callable(getmet):
            return getmet
        return Boolean(False)

    """ OBJECT INFORMATION """
    def getclass(self):
        return IvyObject

    def getobj(self):
        return self

    def gettype(self):
        return self.type

    def istype(self, other):
        if isinstance(other, String):
            if self.type == other.data:
                return Boolean(True)
        return Boolean(False)

    def istrue(self):
        return Boolean(True)

    def isfalse(self):
        return Boolean(not bool(self.istrue()))

    def isnull(self):
        return Boolean(False)

    def getobj(self):
        return self

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
        return String(string, token=self.token, trace=self.trace)

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
    def __init__(self, objdef, name='attr', token=None, trace=None):
        self.type = name
        self.name = name
        self.token = token
        self.trace = trace
        self.attributes = objdef

    def __getitem__(self, att):
        return self.newobj(self.attributes[att])

    def __setitem__(self, att, val):
        self.attributes[att] = val

    def __delitem__(self, att):
        self.attributes.pop()

    def attrget(self, att):
        if att in self.attributes:
            return self.newobj(self.attributes[att])
        return False

    def attrset(self, att, val):
        self.attributes[att] = val

    def attrdel(self, att, val):
        self.attributes.pop(att)

class Null(IvyObject):
    def __init__(self, token=None, trace=None):
        super().__init__(type='Null', token=token, trace=trace)

    def dobinop(self, op, other, searchm=False):
        if 'op_'+op in BINOPMAP.keys():
            self.undefined(op, mes="No binary operation for object of type 'Null' is allowed!")
        self.undefined(op, mes="No operation for object of type 'Null' is allowed!")

    def isnull(self):
        return Boolean(True)

    def istrue(self):
        return Boolean(False)

    def printable(self):
        return self.newprintable('null')

class DataObject(IvyObject):
    def __init__(self, type=None, data=None, token=None, trace=None):
        self.data = data
        super().__init__(type=type, token=token, trace=trace)
        self.objdef['data'] = data

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

    def istrue(self):
        return Boolean(True)

class Integer(DataObject):
    def __init__(self, data, token=None, trace=None):
        super().__init__(type='Integer', data=self.trynew(data, int, 'Integer'), token=token, trace=trace)

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
    def __init__(self, data, token=None, trace=None):
        super().__init__(type='Float', data=self.trynew(data, float, 'Float'), token=token, trace=trace)

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
    def __init__(self, data, token=None, trace=None):
        super().__init__(type='String', data=self.trynew(data, str, 'String'), token=token, trace=trace)
        self.len = len(self.data)
        self.objdef['length'] = len(self.data)

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
    def __init__(self, booldata, token=None, trace=None):
        if booldata:
            booldata = 1
        else:
            booldata = 0
        super().__init__(type='Boolean', data=booldata, token=token, trace=trace)

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

    def istrue(self):
        return Boolean(self.data == 1)

    def __bool__(self):
        return self.data > 0

class Collection(DataObject):
    def __init__(self, data, token=None, trace=None):
        super().__init__(type='Collection', data=data, token=token)
        self.len = len(self.data)
        self.objdef['collection'] = data
        self.objdef['length'] = len(data)

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

    def representation(self):
        pt = '['
        for n,i in enumerate(self.data):
            pt += i.toprint()
            if n!=len(self.data)-1: pt += ', '
        return self.newprintable(pt + ']')

class Block(IvyObject):
    def __init__(self, block):
        self.block = block

    def representation(self):
        return self.newprintable("<Object.Block ")

class Function(IvyObject):
    def __init__(self, params, code, name='<Function>', token=None, trace=None):
        super().__init__(type='Function', token=token, trace=trace)
        self.reference = name
        self.type = 'Function'
        self.name = name
        self.params = params
        self.block = code
        self.native = False
        self.objdef['reference'] = self.reference
        self.objdef['name'] = self.name
        self.objdef['params'] = self.params
        self.objdef['block'] = self.block

    def process_param(param):
        if isinstance(param, String):
            return '"' + str(param.data) + '"'
        elif isinstance(param, Boolean):
            return str(param.__bool__())
        return str(param.data)

    def checkargs(self, args, paramtoken):
        if len(self.params) != len(args):
            self.error(type='Checking Arguments', mes="'{}' takes {} arguments, but {} were given".format(self.name, len(self.params), len(args)),
                       token=paramtoken, etype=IvyCallError)

    def call(self, interpreter, node):
        args = node.list_expr
        cur_record = interpreter.callstack.peek()
        cur_depth = cur_record.depth
        record = interpreter.callstack.copy(Record(self.name, RecordType.FUNCTION, cur_depth+1))
        paramtoken = interpreter.visit(args[0]).token if len(args) != 0 else node.variable
        self.checkargs(args, paramtoken)
        eargs = []
        for n, i in enumerate(self.params):
            val = interpreter.visit(args[n])
            if not self.native:
                record[i.value] = val
            eargs.append(val)
        interpreter.callstack.push(record)
        if self.native:
            call = self.do(eargs)
        else:
            call = interpreter.visit(self.block)
        interpreter.callstack.pop()
        if not isinstance(call, IvyObject):
            obj = self.newobj('null', node.variable)
            # print("newobj")
            # print(obj.istrue)
            return obj
        return call

class Method(Function):
    def __init__(self, boundclass, params, code, name, token=None, trace=None):
        self.bound = boundclass
        self.type = 'Method'
        self.name = name
        self.params = params
        self.block = code
        self.token = token
        self.trace = trace

class BuiltinMethod(Method):
    def __init__(self, boundclass, params=[], name='<BuiltinMethod>', func=None, token=None, trace=None):
        super().__init__(boundclass=boundclass, params=params, code=None, name='<Method>', token=token, trace=trace)
        self.type='BuiltinMethod'
        self.name = name
        self.func = func
        self.native = True

    def do(self, args):
        func = self.func
        params = ','.join([process_param(i) for i in args]) if args != [] else ''
        return eval('func(%s)' % (params))

class BuiltinFunction(Function):
    def __init__(self, trace):
        super().__init__(params=None, code=None, name='<BuiltinFunction>', token=None, trace=trace)
        self.type = 'BuiltinFunction'
        self.native = True

class FunctionType(BuiltinFunction):
    def __init__(self, trace):
        super().__init__(trace)
        self.name = 'type'
        self.params = ['object']

    def do(self, args):
        return args[0].objdef['type']

class FunctionClock(BuiltinFunction):
    def __init__(self, trace):
        super().__init__(trace)
        self.name = 'clock'
        self.params = []

    def do(self, args):
        time = datetime.now()
        return self.newobj(str(time.strftime("%H:%M:%S:%f")))

class FunctionIstrue(BuiltinFunction):
    def __init__(self, trace):
        super().__init__(trace)
        self.name = 'istrue'
        self.params = ['object']

    def do(self, args):
        return args[0].istrue()

class FunctionRepr(BuiltinFunction):
    def __init__(self, trace):
        super().__init__(trace)
        self.name = 'repr'
        self.params = ['object']

    def do(self, args):
        return args[0].representation()

class FunctionPrint(BuiltinFunction):
    def __init__(self, trace):
        super().__init__(trace)
        self.name = 'print'
        self.params = ['object']

    def do(self, args):
        print(args[0].printable().toprint())

class FunctionLength(BuiltinFunction):
    def __init__(self, trace):
        super().__init__(trace)
        self.name = 'length'
        self.params = ['object']

    def do(self, args):
        if hasattr(args[0], 'length'):
            return args[0].length()
        self.error('Accessing Length', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'length'" % (args[0].type), token=args[0].token)
