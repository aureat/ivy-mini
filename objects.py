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
            'obj_system_repr': self.__repr__(),
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
        return False

    def attrset(self, att, val):
        self.objdef[att] = val

    def attrdel(self, att, val):
        self.objdef.pop(att)

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
    def op_mult(self, other): pass
    def op_div(self, other): pass
    def op_add(self, other): pass
    def op_sub(self, other): pass
    def op_pow(self): pass
    def op_eq(self): pass
    def op_lt(self): pass
    def op_lte(self): pass
    def op_gt(self): pass
    def op_gte(self): pass
    def op_in(self): pass

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
        self.__del__()

    def dynamic_package(self): pass

    def printable(self):
        return self.__repr__()

    def __repr__(self):
        return '<%s: %s, %s with 0x%08x>' % (self.getname(), self.obj_name, self.gettype(), id(self))

class Null(IvyObject):
    def __init__(self):
        super().__init__(obj_type='null')

    def isnull(self):
        return True

class DataObject(IvyObject):
    def __init__(self, obj_type, data_type=None, data=None):
        self.data = data
        self.data_type = data_type if data_type is not None else obj_type
        super().__init__(obj_type=obj_type)
        self.objdef.update({
            'obj_data_type': self.data_type,
            'obj_data': data
        })

    def printable(self):
        return self.attrget('obj_data')

class Integer(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Integer', data=data)

class Float(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Float', data=data)

class String(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='String', data=data)

class Boolean(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Boolean', data=data)

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
    def __init__(self, name, params, code):
        super().__init__(obj_name=name,obj_type='func')
        self.objdef.update({
            'obj_params': params,
            'obj_code': code, # stores a code object
        })

    def obj_call(self):
        pass

    def invoke(self):
        self.obj_call()

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
