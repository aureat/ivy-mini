"""
Language Model
func [name] = function( [params] ) { [code] }

Object Model
Object (IvyObject) => ClassObject | ...
  - func (Function)
  - CodeObject (CodeObject)
  - str (String)
  - int (Integer)
  - float (Float)
  - null (Null)
  - coll (Collection)
  - dict (Dictionary)
  - arr (Array)
  - Error (Error)

"""

class IvyObject(object):

    def __init__(self, obj_name=self.__name__, obj_type=self.__name__):
        self.obj_name = name
        self.objdef = {
            'obj_name': obj_name,
            'obj_type': obj_type,
            'obj_class_name': self.__name__,
        }

    def get_prop(self, att):
        try:
            return getattr(self, att)
        except AttributeError:
            return False

    def get_attr(self, att):
        if att in self.objdef:
            return self.objdef[att]
        return False

    def get_type(self):
        return obj_type

    def get_obj(self):
        return self

    def __repr__(self):
        return '<{name}: {obj_name}, {obj_type} at {id}>'.format(self.__name__, self.obj_name, obj_type, id(self))

class DataObject(IvyObject): pass

class Null(IvyObject):
    def __init__(self):
        super().__init__(obj_type='null')

class Integer(IvyObject):
    def __init__(self):
        super().__init__(obj_type='int')

class Float(IvyObject):
    def __init__(self):
        super().__init__(obj_type='float')

class String(IvyObject):
    def __init__(self):
        super().__init__(obj_type='str')

class Collection(IvyObject):
    def __init__(self):
        super().__init__(obj_type='coll')

class Dictionary(IvyObject):
    def __init__(self):
        super().__init__(obj_type='dict')

class Array(IvyObject):
    def __init__(self):
        super().__init__(obj_type='arr')

class Function(IvyObject):
    def __init__(self, name, params, code, ):
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

class IvyClass(IvyObject):
    def __init__(self, obj_type='class'):
        pass

    # User Created Objects will inherit from this class
    # Constructor should have name, attributes, ...
