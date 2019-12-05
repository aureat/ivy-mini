"""
*** SYSTEM OBJECTS

(1) Object Priority: Collection > String > Float > Integer > Boolean

"""

""" OPERATIONS ON OBJECTS """
OPMAP = {
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

class IvyObject(object):

    """ INITIALIZE IVY OBJECT """
    def __init__(self, obj_name=None, obj_type=None):
        name = obj_name if obj_name != None else self.getname()
        self.obj_name = name
        self.objdef = {
            'obj_name': name,
            'obj_type': obj_type if obj_type != None else self.getname(),
            'obj_class_name': self.getname(),
            'obj_self': self.getobj(),
        }

    """ Python Attributes """
    def getprop(self, att):
        return getattr(self, att, False)

    def setprop(self, att, val):
        setattr(self, att, val)

    def delprop(self, att):
        delattr(self, att)

    """ Ivy Object Attributes """
    def attrget(self, att):
        if att in self.objdef:
            return self.objdef[att]
        return self.getprop(att)

    def attrset(self, att, val):
        self.objdef[att] = val

    def attrdel(self, att, val):
        self.objdef.pop(att)

    def getmethod(self, att):
        if att in self.objdef:
            if callable(self.objdef[att]):
                return self.objdef[att]
        getmet = self.getprop(att)
        if callable(getmet):
            return getmet
        return Boolean(False)

    """ Ivy Object General """
    def gettype(self):
        return self.attrget('obj_type')

    def getobj(self):
        return self

    def getname(self):
        return self.__class__.__name__

    def getrepr(self):
        return self.__repr__()

    """ OPERATIONS ON OBJECTS """

    # def op_add(self, other): pass
    # def op_mult(self, other): pass
    # def op_div(self, other): pass
    # def op_sub(self, other): pass
    # def op_mod(self, other): pass
    # def op_pow(self, other): pass
    # def op_lt(self, other): pass
    # def op_lte(self, other): pass
    # def op_gt(self, other): pass
    # def op_gte(self, other): pass
    # def op_eq(self, other): pass
    # def op_eq_not(self, other): pass
    # def op_in(self, other): pass

    def op_ideq(self, other):
        return Boolean(self.gettype() == other.gettype())

    def op_ideq_not(self, other):
        return Boolean(self.gettype() != other.gettype())

    """ OBJECT PROPERTIES """
    def istrue(self): pass
    def isnull(self): pass

    """ LIST """
    def getitem(self): pass
    def additem(self): pass
    def delitem(self): pass
    def length(self): pass

    def isfalse(self):
        return not self.istrue()

    def kill(self):
        return Null()

    def dynamic_package(self): pass

    def getop(self, attr, other):
        return self.define_op(attr, other)

    def printable(self):
        return self.__repr__()

    def undefined(self):
        print("Undefined operation")

    def __repr__(self):
        return '<IvyObject.%s at 0x%08x>' % (self.gettype(), id(self))

class Null(IvyObject):
    def __init__(self):
        super().__init__(obj_type='Null')

    def define_op(self, op, other, searchm=False):
        if 'op_'+op in OPMAP.keys():
            print("No binary operation on a Null object is allowed")

    def isnull(self):
        return True

    def printable(self):
        return 'null'

class DataObject(IvyObject):
    def __init__(self, obj_type, data_type=None, data=None):
        self.data = data
        self.data_type = data_type if data_type is not None else obj_type
        super().__init__(obj_type=obj_type)
        self.objdef.update({
            'obj_data_type': self.data_type,
            'obj_data': data
        })
        if data == None:
            self.kill()

    def op_lt(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data > other.data)
        elif isinstance(other, Float):
            return Boolean(self.data > other.data)
        self.undefined()

    def op_lte(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data >= other.data)
        elif isinstance(other, Float):
            return Boolean(self.data >= other.data)
        self.undefined()

    def op_gt(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data < other.data)
        elif isinstance(other, Float):
            return Boolean(self.data < other.data)
        self.undefined()

    def op_gte(self, other):
        if isinstance(other, Integer):
            return Boolean(self.data <= other.data)
        elif isinstance(other, Float):
            return Boolean(self.data <= other.data)
        self.undefined()

    def define_op(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Integer):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Float):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, String):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        if not searchm:
            oper = other.define_op(op, other, searchm=True)
            if oper != False:
                return oper
            print('Error: Binary operation not defined for these two types')
        return False

    def op_result(self, data):
        self.data = data
        self.objdef.update({
            'obj_data': data
        })
        return self

    def op_eq(self, other):
        if getattr(other, 'data', False) != False:
            return Boolean(self.data == other.data)
        return Boolean(self.data == other)

    def op_eq_not(self, other):
        if getattr(other, 'data', False) != False:
            return Boolean(self.data != other.data)
        return Boolean(self.data != other)

    def isnull(self):
        return self.data == None

    def printable(self):
        return self.attrget('obj_data')

    def __repr__(self):
        return '<IvyObject.DataObject.%s (data=%s) at 0x%08x>' % (self.gettype(), self.data, id(self))

class Integer(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Integer', data=data)

    def op_plus(self):
        return Integer(self.data)

    def op_minus(self):
        return Integer(-self.data)

    def op_not(self):
        self.undefined()

    def istrue(self):
        return Boolean(True) if abs(self.data) > 0 else Boolean(False)

    def define_op(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(OPMAP['op_' + op](self.data, other.data))
        elif isinstance(other, Integer):
            return self.op_result(OPMAP['op_' + op](self.data, other.data))
        if not searchm:
            oper = other.define_op(op, self, searchm=True)
            if oper != False:
                return oper
            print('Error: Binary operation not defined for these two types')
        return False

    def op_result(self, data):
        return Integer(data)

class Float(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Float', data=data)

    def op_plus(self):
        return Integer(self.data)

    def op_minus(self):
        return Integer(-self.data)

    def op_not(self):
        self.undefined()

    def istrue(self, other):
        return Boolean(True) if abs(self.data) > 0 else Boolean(False)

    def define_op(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(OPMAP['op_' + op](self.data, other.data))
        elif isinstance(other, Integer):
            return self.op_result(OPMAP['op_' + op](self.data, other.data))
        elif isinstance(other, Float):
            return self.op_result(OPMAP['op_' + op](self.data, other.data))
        if not searchm:
            oper = other.define_op(op, other, searchm=True)
            if oper != False:
                return oper
            print('Error: Binary operation not defined for these two types')
        return False

    def op_result(self, data):
        return Float(data)

class String(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='String', data=data)
        self.objdef.update({
            'length': len(self.data)
        })

    def op_plus(self):
        self.undefined()

    def op_minus(self):
        self.undefined()

    def op_not(self):
        self.undefined()

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

    def define_op(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Integer):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, Float):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        elif isinstance(other, String):
            return self.op_result(OPMAP['op_' + op](str(self.data), str(other.data)))
        if not searchm:
            oper = other.define_op(op, other, searchm=True)
            if oper != False:
                return oper
            print('Error: Binary operation not defined for these two types')
        return False

    def op_result(self, data):
        return String(data)

class Boolean(DataObject):
    def __init__(self, booldata):
        if booldata:
            booldata = 1
        else:
            booldata = 0
        super().__init__(obj_type='Boolean', data=booldata)

    def op_plus(self):
        self.undefined()

    def op_minus(self):
        self.undefined()

    def op_not(self):
        return Boolean(not bool(self.data))

    def op_and(self, other):
        return Boolean(bool(self.data) and bool(other.data))

    def op_or(self, other):
        return Boolean(bool(self.data) and bool(other.data))

    def define_op(self, op, other, searchm=False):
        selfattr = getattr(self, 'op_'+op, False)
        if selfattr != False:
            print("op exists")
            return selfattr(other)
        if isinstance(other, Boolean):
            return self.op_result(OPMAP['op_' + op](self.data, other.data))
        if not searchm:
            oper = other.define_op(op, self, searchm=True)
            if oper != False:
                return oper
            print('Error: Binary operation not defined for these two types')
        return False

    def op_result(self, data):
        return Boolean(data)

    def printable(self):
        if self.data:
            return 'true'
        else:
            return 'false'

    def istrue(self):
        return Boolean(self.data > 0)

    def __bool__(self):
        return self.data > 0

class TrueBoolean(Boolean):
    def __init__(self):
        super().__init__(booldata=1)

class FalseBoolean(Boolean):
    def __init__(self):
        super().__init__(booldata=0)

class Collection(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='coll', data=data)
        self.objdef.update({
            'obj_coll': []
        })

    def getitem(self, ref):
        return self.objdef['obj_coll'][ref]

    def additem(self, val):
        self.objdef['obj_coll'].append(val)

    def delitem(self, val):
        self.objdef['obj_coll'].remove(val)

class Dictionary(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='dict', data=data)

class Array(DataObject):
    def __init__(self,data):
        super().__init__(obj_type='arr', data=data)

class Function(IvyObject):
    def __init__(self, params, code, name='<Function>'):
        super().__init__(obj_type='Function')
        self.name = name
        self.params = params
        self.block = code
        self.objdef.update({
            'obj_params': params,
            'obj_code': code, # stores a code object
        })

class CodeObject(IvyObject):

    def __init__(self, code):
        super().__init__()
        self.objdef.update({
            'obj_code': code,
        })

    def invoke(self):
        pass

class Block:
    def __init__(self, block):
        self.block = block

class IvyClass(IvyObject):
    def __init__(self, obj_type='struct'):
        pass
