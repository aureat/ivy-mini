from src.objects import *

class BuiltinFunction(Function):
    def __init__(self, trace, interpreter=None):
        super().__init__(params=None, code=None, name='<BuiltinFunction>', token=None, trace=trace, interpreter=interpreter)
        self.type = 'BuiltinFunction'
        self.native = True


""" LIBRARY FUNCTIONS """
class FunctionPrint(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'print'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        print(args[0].printable().toprint())

class FunctionType(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'type'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        return args[0].objdef['type']

class FunctionClock(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'clock'
        self.params = []

    def do(self, args, params, interpreter, node):
        time = datetime.now()
        return self.newobj(str(time.strftime("%H:%M:%S:%f")))

""" ATTRIBUTE FUNCTIONS """
class FunctionIstrue(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'istrue'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('istrue'):
            func = args[0].attrget('istrue')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'istrue'" % (args[0].type), token=args[0].token)

class FunctionIsnull(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'isnull'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('isnull'):
            func = args[0].attrget('isnull')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'isnull'" % (args[0].type), token=args[0].token)

class FunctionIstype(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'istype'
        self.params = ['object', 'type']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('istype'):
            func = args[0].attrget('istype')
            return self.interpreter.call(func, self.token, params[1:])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'istype'" % (args[0].type), token=args[0].token)

class FunctionRepr(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'repr'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('repr'):
            func = args[0].attrget('repr')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'repr'" % (args[0].type), token=args[0].token)

class FunctionLength(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'length'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('length'):
            func = args[0].attrget('length')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'length'" % (args[0].type), token=args[0].token)

class FunctionCallable(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'callable'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('callable'):
            func = args[0].attrget('callable')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'callable'" % (args[0].type), token=args[0].token)

class FunctionIndexable(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'indexable'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('indexable'):
            func = args[0].attrget('indexable')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'indexable'" % (args[0].type), token=args[0].token)

class FunctionAttrget(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'attrget'
        self.params = ['object', 'attr']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('attrget'):
            func = args[0].attrget('attrget')
            return self.interpreter.call(func, self.token, params[1:])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'attrget'" % (args[0].type), token=args[0].token)

class FunctionAttrhas(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'attrhas'
        self.params = ['object', 'attr']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('attrhas'):
            func = args[0].attrget('attrhas')
            return self.interpreter.call(func, self.token, params[1:])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'attrhas'" % (args[0].type), token=args[0].token)

class FunctionAttrset(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'attrset'
        self.params = ['object', 'attr', 'value']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('attrset'):
            func = args[0].attrget('attrset')
            return self.interpreter.call(func, self.token, params[1:])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'attrset'" % (args[0].type), token=args[0].token)

class FunctionPrintable(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'printable'
        self.params = ['object']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('printable'):
            func = args[0].attrget('printable')
            return self.interpreter.call(func, self.token, [])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'printable'" % (args[0].type), token=args[0].token)

class FunctionAttrdel(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'attrdel'
        self.params = ['object', 'attr']

    def do(self, args, params, interpreter, node):
        if args[0].attrhas('attrdel'):
            func = args[0].attrget('attrdel')
            return self.interpreter.call(func, self.token, params[1:])
        self.error('Accessing attribute', etype=IvyAttributeError, mes="Object of type '%s' does not have attribute 'attrdel'" % (args[0].type), token=args[0].token)

class FunctionExit(BuiltinFunction):
    def __init__(self, trace, interpreter=None):
        super().__init__(trace, interpreter=interpreter)
        self.name = 'exit'
        self.params = ['mes']

    def do(self, args, params, interpreter, node):
        exit(args[0]);
