"""
***  IVY Language Interpreter
***  Interpreter

TODO:
- implement a universal type system for structures and type objects
  and fix errors like "object of type ... does no.."
- implement self for structures
- static attributes for an object (construct function is also a static function)
- for loops do have their own scope, with the name defined inside the for loop
- FUNCTION SIGNATURES (?):
      - func.method.call.call() stuff like this don't work since the python methods
        are called and returned as a function wrapper, and their call methods have
        an parameter signature of 1.
      - fix function modeling and method handling
- Type object as struct instances
    - magic methods and attributes _name, _type, _struct, _model
        _add, _mult, _repr, _string, _getattr, _hasattr, _delattr, setattr, etc.
- Struct Modification capability, extensions, display numbers in a different way for example
    struct Integer (from Integer) {
        function string() {
            return "this integer's value is " + self.value;
        }
    }

tests
- interesting stuff with attribute modification for structs
and struct extensions : struct Object (from Object)
- modify method of a struct, replace with a function
expression bound to another scope, scope binding is buggy

"""

import sys
from src.ast import *
from src.error import *
from src.objs import *
from src.tokentype import TokenType
from src.env import Env
from src.lexer import Lexer
from src.parser import Parser
from src.resolver import Resolver
from src.files import IOWrapper

class obj(object):
     def test(): pass
def dummy(): pass
pycallables = [type(print), type(dummy), type(obj().test)]
def isfunc(obj):
    return type(obj) in pycallables

class Interpreter:
    def __init__(self, trace, console_mode=False):
        self.console_mode = console_mode
        self.trace = trace
        self.globals = Env(interp=self)
        self.env = self.globals
        self.locals = {}
        self.init_globals()

    def init_globals(self):
        # Object
        StructObject = Object()
        StructObject.initenv(self.env)
        # StructObject.setparent(StructObject)
        # Integer
        # StructInteger = Integer()
        # StructInteger.setparent(StructObject)
        # StructInteger.setparentenv()
        # StructFunction = Function()
        # StructFunction.setparent(StructObject)
        self.globals['Object'] = StructObject
        # self.globals['Integer'] = StructInteger
        # self.globals['Function'] = StructFunction
        self.globals['print'] = FunctionWrapper(self.func_println)
        self.globals['exit'] = FunctionWrapper(self.exit)
        self.globals['import'] = FunctionWrapper(self.import_file)
        self.globals['length'] = FunctionWrapper(self.func_length)
        self.globals['globals'] = FunctionWrapper(self.func_globals)
        self.globals['locals'] = FunctionWrapper(self.func_locals)

    """ SOME BUILTINS """
    def func_println(self, obj):
        if isinstance(obj, Wrapper):
            res = self.visit(obj.getattr("string")).call()
        elif obj is True:
            res = "true"
        elif obj is False:
            res = "false"
        elif obj is None:
            res = "null"
        else:
            res = self.visit(obj)
        print(res)

    def func_globals(self):
        return dict(self.globals)

    def func_locals(self):
        return dict(self.env)

    def func_length(self, obj):
        return len(obj)

    def exit(self):
        sys.exit(0)

    def import_file(self, file):
        trace = self.trace
        lexer = Lexer(trace=trace)
        parser = Parser(trace=trace)
        interpreter = Interpreter(trace)
        interpreter.globals = self.globals
        resolver = Resolver(interpreter)
        try:
            io = IOWrapper(trace)
            io.frompath(file)
            file = io.newfile()
            tokens = lexer.tokenizefile(file)
            tree = parser.parse(file, tokens)
            resolver.resolve(file, tree)
            res = interpreter.interpret(file, tree)
        except Error as e:
            print(e)

    def call(self, obj, *args):
        if self.isobj(obj):
            if obj.hasattr('call'):
                return obj.call(*args)
        token = obj.token if hasattr(obj, 'token') else None
        self.error(mes="Object '%s' not callable" % (self.typeof(obj)), token=token, etype=IvyCallError)

    """ INTERPRETER RESULT """
    def load(self, file, tree):
        self.file = file
        self.tree = tree

    def interpret(self, file, tree):
        self.load(file, tree)
        if self.tree is None:
            exc = None
        else:
            exc = self.visit(tree)
        return exc

    def newobj(self, obj):
        if isfunc(obj):
            func = FunctionWrapper(obj)
            func.interp = self
            return func
        return obj

    """ ERROR HANDLING """
    def error(self, mes, token=None, etype=IvyRuntimeError):
        self.trace.add(type='Runtime', token=token, framename=self.env.name, file=self.file)
        raise etype(mes, self.trace)

    def notfound(self, node):
        self.error(mes="Cannot execute code", etype=IvyRuntimeError)

    """ ENVIRONMENT """
    def getdepth(self, expr):
        return self.locals.get(expr)

    def resolve(self, expr, depth):
        self.locals[expr] = depth

    def lookup(self, name, expr):
        dist = self.getdepth(expr)
        if dist is not None:
            return self.env.look_at(dist, name)
        return self.globals.lookup(name)

    def assign(self, name, value, expr):
        dist = self.getdepth(expr)
        if dist is not None:
            self.env.assign_at(dist, name, value)
        else:
            self.globals.assign(name, value)

    """ PARSE TREE NAVIGATOR """
    def visit(self, node):
        cls = type(node).__name__
        method_name = 'visit_' + cls
        visitor = getattr(self, method_name, None)
        if visitor is None:
            return self.newobj(node)
        return self.newobj(visitor(node))

    def visit_IvyObject(self, node): pass

    def visit_TypeObject(self, node):
        return node

    """ OBJECT HELPERS """
    def isobj(self, obj):
        if isinstance(obj, Wrapper):
            return True
        return False

    def typeof(self, obj):
        if isinstance(obj, Wrapper):
            return self.newobj(obj['type'])
        return type(obj).__name__

    def getattr(self, obj, name):
        if hasattr(obj, 'getattr'):
            exc = self.visit(obj.getattr(name))
            return exc
        self.error(mes="Object does not define a 'getattr' method",etype=IvyAttributeError)

    def istrue(self, obj): pass
    def isnull(self, obj): pass
    def issame(self, obj): pass
    def isequal(self, obj): pass

    """ COMPOUNDS """
    def visit_Program(self, node):
        self.visit(node.block)

    def execute_block(self, node, environment=None):
        env = self.env
        try:
            self.env = environment
            self.visit(node.block)
        finally:
            self.env = env

    def visit_Block(self, node):
        env = Env(node.name, self.env) if node.scope else self.env
        self.execute_block(node, env)

    def visit_Compound(self, node):
        for i in node.list:
            self.visit(i)

    """ STATEMENTS """
    def visit_ExpressionStatement(self, node):
        exc = self.visit(node.expr)
        if self.console_mode:
            self.func_println(exc)

    def visit_Assignment(self, node):
        value = self.visit(node.value)
        if isinstance(value, Function):
            value.reference = node.id.value
        self.assign(node.id.value, value, node)
        return (node.id.value, value)

    def visit_ReturnStatement(self, node):
        expr = self.visit(node.expr) if node.expr is not None else None
        raise ReturnValue(expr)

    def visit_AssertStatement(self, node):
        if not self.visit(node.expr):
            self.error(mes='Statement not valid', token=node.token, etype=IvyAssertionError)

    def visit_DeleteStatement(self, node):
        name = node.name.value
        dist = self.getdepth(node)
        if dist is not None:
            self.env.remove_at(dist, name)
        elif name in self.globals:
            self.globals.pop(name)

    """ EXPRESSIONS """
    def visit_VariableCall(self, node):
        try:
            return self.lookup(node.callname, node)
        except IvyNameError as e:
            self.error(mes="Name '%s' is not defined" % (node.callname), token=node.token, etype=IvyNameError)

    def visit_IndexCall(self, node):
        var = self.visit(node.variable)
        ind = self.visit(node.index)
        return var[ind]

    # def visit_BinaryOperator(self, node):
    #     left = self.visit(node.left)
    #     right = self.visit(node.right)
    #     if node.op.type == TokenType.PLUS:
    #         if left.hasattr('_add'):
    #             result = self.call(self.getattr(left, '_add'), right)
    #         else:
    #             result = self.call(self.getattr(right, '_add'), left)
    #     elif node.op.type == TokenType.MINUS:
    #         result = self.call(self.getattr(left, '_minus'), right)
    #     elif node.op.type == TokenType.MUL:
    #         if left.hasattr('_mul'):
    #             result = self.call(self.getattr(left, '_mul'), right)
    #         else:
    #             result = self.call(self.getattr(right, '_mul'), left)
    #     elif node.op.type == TokenType.FLOAT_DIV:
    #         result = self.call(self.getattr(left, '_fdiv'), right)
    #     elif node.op.type == TokenType.MOD:
    #         result = self.call(self.getattr(left, '_mod'), right)
    #     elif node.op.type == TokenType.POWER:
    #         result = self.call(self.getattr(left, '_pow'), right)
    #     elif node.op.type == TokenType.COMP_EQ:
    #         if left.hasattr('_eq'):
    #             result = self.call(self.getattr(left, '_eq'), right)
    #         else:
    #             result = self.call(self.getattr(right, '_eq'), left)
    #     elif node.op.type == TokenType.COMP_EQ_NOT:
    #         if left.hasattr('_noteq'):
    #             result = self.call(self.getattr(left, '_noteq'), right)
    #         else:
    #             result = self.call(self.getattr(right, '_noteq'), left)
    #     elif node.op.type == TokenType.COMP_GT:
    #         result = self.call(self.getattr(left, '_gt'), right)
    #     elif node.op.type == TokenType.COMP_GTE:
    #         result = self.call(self.getattr(left, '_gte'), right)
    #     elif node.op.type == TokenType.COMP_LT:
    #         result = self.call(self.getattr(left, '_lt'), right)
    #     elif node.op.type == TokenType.COMP_LTE:
    #         result = self.call(self.getattr(left, '_lte'), right)
    #     elif node.op.type == TokenType.AND: # Short-circuited
    #         if left.hasattr('_and'):
    #             if not left.istrue(): return Boolean(False)
    #             result = self.call(self.getattr(left, '_and'), right)
    #         else:
    #             if not right.istrue(): return Boolean(False)
    #             result = self.call(self.getattr(right, '_and'), left)
    #     elif node.op.type == TokenType.OR: # Short-circuited
    #         if left.hasattr('_or'):
    #             if left.istrue(): return Boolean(True)
    #             result = self.call(self.getattr(left, '_or'), right)
    #         else:
    #             if right.istrue(): return Boolean(True)
    #             result = self.call(self.getattr(right, '_or'), left)
    #     elif node.op.type == TokenType.COMP_ID:
    #         if left.hasattr('_ideq'):
    #             result = self.call(self.getattr(left, '_ideq'), right)
    #         else:
    #             result = self.call(self.getattr(right, '_ideq'), left)
    #     elif node.op.type == TokenType.COMP_ID_NOT:
    #         if left.hasattr('_notideq'):
    #             result = self.call(self.getattr(left, '_notideq'), right)
    #         else:
    #             result = self.call(self.getattr(right, '_notideq'), left)
    #     self.env.define('_', result)
    #     return result

    def visit_BinaryOperator(self, node):
        left = self.visit(node.left)
        right = self.visit(node.right)
        if node.op.type == TokenType.PLUS:
            result = left + right
        elif node.op.type == TokenType.MINUS:
            result = left - right
        elif node.op.type == TokenType.MUL:
            result = left * right
        elif node.op.type == TokenType.FLOAT_DIV:
            result = left / right
        elif node.op.type == TokenType.MOD:
            result = left % right
        elif node.op.type == TokenType.POWER:
            result = left ** right
        elif node.op.type == TokenType.COMP_EQ:
            result = left == right
        elif node.op.type == TokenType.COMP_EQ_NOT:
            result = left != right
        elif node.op.type == TokenType.COMP_GT:
            result = left < right
        elif node.op.type == TokenType.COMP_GTE:
            result = left <= right
        elif node.op.type == TokenType.COMP_LT:
            result = left > right
        elif node.op.type == TokenType.COMP_LTE:
            result = left >= right
        elif node.op.type == TokenType.AND: # Short-circuited
            if not left: return False
            result = left and right
        elif node.op.type == TokenType.OR: # Short-circuited
            if left: return True
            result = left or right
        elif node.op.type == TokenType.COMP_ID:
            result = type(left) == type(right)
        elif node.op.type == TokenType.COMP_ID_NOT:
            result = type(left) != type(right)
        self.env.define('_', result)
        return result

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == TokenType.PLUS:
            return self.visit(node.expr)
        elif op == TokenType.MINUS:
            return -self.visit(node.expr)
        elif op == TokenType.NOT:
            return not self.visit(node.expr)

    """ FUNCTIONS """
    def visit_FunctionDeclaration(self, node):
        func = self.visit(node.func)
        self.env.define(node.func.name, func)
        return (node.func.name, func)

    def visit_Function(self, node):
        if node.closure is None:
            node.closure = self.env
        node.interp = self
        return node

    def call_function(self, func, *args):
        caller = self.visit(func.getattr('call'))
        if len(args) != caller.args:
            self.error(mes="'%s' expected %s arguments, but %s were given" % (caller.name, caller.args, len(args)),
                       token=node.token, etype=IvyCallError)
        return caller.call(*args)

    def visit_FunctionCall(self, node):
        func = self.visit(node.variable)
        args = [self.visit(arg) for arg in node.list_expr] if node.list_expr else []
        caller = self.visit(func.getattr('call'))
        if not isinstance(caller, FunctionWrapper):
            if len(args) != caller.args:
                self.error(mes="'%s' expected %s arguments, but %s were given" % (caller.name, caller.args, len(args)),
                           token=node.token, etype=IvyCallError)
        return self.visit(caller.call(*args))

    """ STRUCTURES """
    def visit_StructDeclaration(self, node):
        name = node.name.value
        super = self.globals['Object']
        if node.super is not None:
            super = self.visit(node.super)
            if not isinstance(super, Struct):
                self.error(mes='Structures can only inherit from other structures', token=node.name)
        self.env = Env(super.name, super.env)
        # self.env = super.env
        self.env.define("super", super)
        # Interpretation of struct blocks: static functions are compiled first,
        # and then the methods are bounded to the scope of the struct and compiled
        # at last the rest of the block excluding methods is compiled
        self.env = Env(name, self.env)
        struct = Struct()
        struct.setparent(super)
        struct.setname(name)
        struct.env = self.env
        struct.block = node.block
        self.env.define('obj', struct)
        for decl in node.statics:
            self.visit_StaticDeclaration(decl, struct)
        # self.env.define('self', struct)
        for decl in node.attributes:
            self.visit_MethodDeclaration(decl, struct)
        self.env = self.env.parent.parent.parent
        self.env.define(name, struct)

    def visit_StaticDeclaration(self, node, struct=None):
        name, value = self.visit(node.node)
        struct.setstatic(name, value)

    def visit_MethodDeclaration(self, node, struct=None):
        method = self.visit(node.decl.func)
        name = node.decl.name
        struct.setattr(name, method)

    def visit_ConstructNew(self, node):
        struct = self.visit(node.variable)
        # print(struct.super.getattr("construct"))
        if not isinstance(struct, Struct):
            self.error(mes='Only structures can be constructed',token=node.token,etype=IvyTypeError)
        try:
            constructor = struct.getattr('construct')
        except IvyAttributeError:
            self.error("Structure '%s' does not have a constructor" % (struct.name),token=node.token,etype=IvyCallError)
        args = [self.visit(arg) for arg in node.list_expr] if node.list_expr else []
        # if len(args) != constructor.args:
        #     self.error(mes="'%s' expected %s arguments, but %s were given" % (constructor.name, constructor.args, len(args)),
        #                token=node.token, etype=IvyCallError)
        enclosing = self.env
        self.env = struct.env
        instance = Instance(struct)
        self.env.define('self', instance)
        instance.construct(*args)
        self.visit(struct.block)
        self.env = enclosing
        return instance

    """ ATTRIBUTES """
    def visit_AttributeCall(self, node, access=False):
        var = self.visit(node.variable)
        try:
            attr = self.getattr(var, node.attribute.value)
            if attr:
                return attr
            self.error(mes="Object of type '%s' does not have the attribute '%s'" % (self.typeof(var), node.attribute.value),
                       token=node.attribute,
                       etype=IvyAttributeError)
        except IvyAttributeError:
            self.error(mes="Object of type '%s' does not have the attribute '%s'" % (self.typeof(var), node.attribute.value),
                       token=node.attribute,
                       etype=IvyAttributeError)

    def visit_AttributeSet(self, node):
        att = node.attribute.value
        var = self.visit(node.variable.variable)
        value = self.visit(node.value)
        var.setattr(att, value)

    """ CONTROL FLOW """
    def visit_Conditional(self, node):
        if self.visit(node.condition): #.istrue()
            self.visit(node.ifblock)
        elif node.elseblock is not None:
            self.visit(node.elseblock)

    def visit_ForLoop(self, node):
        pass

    def visit_WhileLoop(self, node):
        while self.visit(node.condition): #.istrue()
            try:
                exc = self.visit(node.block)
            except BreakNode:
                break
            except ContinueNode:
                continue

    def visit_BreakLoop(self, node):
        raise BreakNode()

    def visit_ContinueLoop(self, node):
        raise ContinueNode()
