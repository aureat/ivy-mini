"""
***  IVY Language Interpreter
***  Semantic Analyzer (Resolver)

* Globals are not resolved: variables are not inserted nor looked up
in the global symbol table.
* Blocks created by control statements do not have their own namespace
* There are no name declarations in ivy
* Structs have static methods and bound methods
    - Bound methods are bound to the closing environment around
      it. So Object1().type = function() {  }

TODO:
* bind anonymous functions to the scope of the struct

"""

from enum import Enum
from src.error import IvyNameError, IvyRuntimeError, IvySyntaxError
from src.objs import Function

class Func(Enum):
    NONE = object()
    CONSTRUCTOR = object()
    METHOD = object()
    FUNCTION = object()

class Struct(Enum):
    NONE = object()
    STRUCT = object()
    CHILD = object()

class SymbolType(Enum):
    NONE = object()
    DECLARED = object()
    DEFINED = object()
    OUTER = object()
    INNER = object()

class Symbol:
    def __init__(self, type, depth=0):
        self.type = type
        self.depth = depth

class SymbolTable:
    def __init__(self, name, depth, parent=None):
        self.symbols = {}
        self.name = name
        self.depth = depth
        self.parent = parent
        if parent is None:
            self.globals = {}

    def child(self, name='<local>'):
        return SymbolTable(name, self.depth+1, self)

    def insert(self, name, symbol):
        self.symbols[name] = symbol

    def remove(self, name):
        self.symbols.pop(name)

    def lookup(self, name, this=False):
        symbol = self.symbols.get(name)
        if symbol is not None:
            return symbol
        if this:
            return Symbol(SymbolType.NONE, self.depth)
        if self.parent is not None:
            return self.parent.lookup(name)

class Resolver:
    def __init__(self, interp):
        self.interp = interp
        self.trace = interp.trace
        self.scope = SymbolTable('<global>', 0, None)
        self.functype = Func.NONE
        self.structtype = Struct.NONE
        self.curstruct = None
        self.isloop = False

    def resolve(self, file, tree):
        self.file = file
        self.tree = tree
        self.visit(tree)

    def error(self, mes, token=None, etype=IvyRuntimeError):
        name = self.scope.name
        self.trace.add(type='Resolution', token=token, framename=name, file=self.file)
        raise etype(mes, self.trace)

    # Resolution Helpers
    def new_scope(self, name=None):
        name = '<local:' + str(self.scope.depth+1) + '>' if name is None else name
        self.scope = self.scope.child(name)

    def close_scope(self):
        self.scope = self.scope.parent

    def localscope(self):
        return self.scope.depth > 0

    def declare(self, name):
        self.scope.insert(name, Symbol(SymbolType.DECLARED))

    def define(self, name):
        self.scope.insert(name, Symbol(SymbolType.DEFINED))

    def outer(self, name, depth):
        self.scope.insert(name, Symbol(SymbolType.OUTER, depth))

    def isdeclared(self, sym):
        return sym.type is SymbolType.DECLARED

    def isdefined(self, sym):
        return sym.type is SymbolType.DEFINED

    def isouter(self, sym):
        return sym.type is SymbolType.OUTER

    def isnone(self, sym):
        return sym.type is SymbolType.NONE

    def resolve_at(self, node, name):
        sym = self.scope.lookup(name, True)
        if sym.type == SymbolType.OUTER:
            depth = sym.depth
            self.interp.resolve(node, depth)
            return True
        return None

    def resolve_local(self, node, name):
        ## Resolve at
        res = self.resolve_at(node, name)
        if res == True: return
        # Resolve by finding
        sym = self.scope.lookup(name, True)
        scoping = self.scope
        counter = 0
        while scoping != None:
            if name in scoping.symbols:
                self.interp.resolve(node, counter)
                break
            counter += 1
            scoping = scoping.parent

    def resolve_outer(self, node, name):
        if not self.localscope():
            self.error(mes='Outer statement in global scope', token=node.token)
        found = False
        scoping = self.scope.parent
        counter = 1
        while scoping is not None:
            if name in scoping.symbols and scoping.lookup(name, this=True).type != SymbolType.OUTER:
                found = True
                break
            counter += 1
            scoping = scoping.parent
        if not found or scoping.depth == 0:
            self.error(mes="Nonglobal outer name '%s' not defined" % (name), token=node.token)
        self.outer(name, counter)
        self.interp.resolve(node, counter)

    def resolve_function(self, node, type):
        enclosing = self.functype
        self.functype = type
        # if self.functype in [Func.METHOD, Func.CONSTRUCTOR]:
        #     self.addmethod(node)
        # function expression references differ from that of function statements
        name = node.reference if node.anonymous else node.name
        self.new_scope(name)
        for param in node.params:
            self.declare(param.value)
            self.define(param.value)
        self.visit(node.block.block)
        self.close_scope()
        self.functype = enclosing

    def visit(self, node):
        visitor = getattr(self, 'visit_' + node.__class__.__name__, False)
        if not visitor:
            return
        return visitor(node)

    # Center of Action for Resolutions
    def visit_Assignment(self, node):
        name = node.id.value
        symin = self.scope.lookup(name, True)
        sym = self.scope.lookup(name)
        if isinstance(node.value, Function):
            node.value.name = node.value.reference = name
            node.value.anonymous = False
        if self.isouter(symin):
            self.visit(node.value)
            self.interp.resolve(node, symin.depth)
        elif self.isnone(symin):
            self.declare(node.id.value)
            self.visit(node.value)
            self.define(node.id.value)
            self.interp.resolve(node, 0)
        else:
            self.visit(node.value)
            self.resolve_local(node, name)
        # self.error(mes="Error assigning to '%s'" % (name),token=node.id)

    def visit_OuterStatement(self, node):
        self.resolve_outer(node, node.name.value)

    def visit_VariableCall(self, node):
        if self.localscope() and self.isdeclared(self.scope.lookup(node.token.value, True)):
                self.error(mes="Name referenced before assignment",token=node.token,etype=IvyNameError)
        self.resolve_local(node, node.token.value)

    def visit_DeleteStatement(self, node):
        self.resolve_local(node, node.name.value)
        depth = self.interp.getdepth(node)
        scope = self.scope
        for i in range(depth):
            scope = scope.parent
        scope.remove(node.name.value)

    def visit_FunctionDeclaration(self, node):
        self.declare(node.name)
        self.define(node.name)
        self.visit(node.func)

    def visit_MethodDeclaration(self, node):
        self.visit(node.decl.func)

    def visit_Function(self, node):
        if node.name or node.name != '<Function>':
            node.anonymous = False
        if self.structtype in [Struct.STRUCT, Struct.CHILD]:
            if node.name == "construct":
                node.setattr('isconstructor', True)
                self.resolve_function(node, Func.CONSTRUCTOR)
                return
            self.resolve_function(node, Func.METHOD)
            return
        self.resolve_function(node, Func.FUNCTION)

    def visit_StaticDeclaration(self, node):
        if not self.structtype in [Struct.STRUCT, Struct.CHILD]:
            self.error(mes='Static declaration outside structs', token=node.token)
        self.visit(node.node)

    def visit_StructDeclaration(self, node):
        self.declare(node.name)
        self.define(node.name)
        enclosing = self.structtype
        curstruct = self.curstruct
        self.curstruct = node
        if node.super is not None:
            self.structtype = Struct.CHILD
            self.visit(node.super)
            self.new_scope()
            self.define('super')
        else:
            self.structtype = Struct.STRUCT
        self.new_scope()
        self.define('obj')
        for static in node.statics:
            self.visit(static)
        self.define('self')
        for decl in node.attributes:
            self.visit(decl)
        self.visit_Block(node.block)
        self.close_scope()
        if node.super is not None:
            self.close_scope()
        self.structtype = enclosing
        self.curstruct = curstruct

    def visit_ConstructNew(self, node):
        newtok = node.token
        object = self.visit(node.variable)
        # error if object does not have a constructor

    # Passing resolutions to children
    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        if node.scope: self.new_scope()
        self.visit(node.block)
        if node.scope: self.close_scope()

    def visit_Compound(self, node):
        for i in node.list:
            self.visit(i)

    def visit_FunctionCall(self, node):
        self.visit(node.variable)
        for i in node.list_expr:
            self.visit(i)

    def visit_BinaryOperator(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Conditional(self, node):
        self.visit(node.condition)
        self.visit(node.ifblock)
        if node.elseblock:
            self.visit(node.elseblock)

    def visit_UnaryOp(self, node):
        self.visit(node.expr)

    def visit_WhileLoop(self, node):
        self.isloop = True
        self.visit(node.condition)
        self.visit(node.block)
        self.isloop = False

    def visit_ForLoop(self, node):
        self.isloop = True
        self.visit(node.iterable)
        self.visit(node.block)
        self.isloop = False

    def visit_BreakLoop(self, node):
        if not self.isloop:
            self.error(mes="Break statement should be inside of loop",
                       token=node.token, etype=IvySyntaxError)

    def visit_ContinueLoop(self, node):
        if not self.isloop:
            self.error(mes="Continue statement should be inside of loop",
                       token=node.token, etype=IvySyntaxError)

    def visit_ReturnStatement(self, node):
        if self.functype == Func.CONSTRUCTOR:
            self.error(mes="Return statement inside a constructor",
                       token=node.token, etype=IvySyntaxError)
        if self.functype not in [Func.FUNCTION, Func.METHOD, Func.CONSTRUCTOR]:
            self.error(mes="Return statement should be inside of function",
                       token=node.token, etype=IvySyntaxError)
        self.visit(node.expr)

    def visit_AttributeCall(self, node):
        self.visit(node.variable)

    def visit_AttributeAccess(self, node):
        self.visit(node)

    def visit_AttributeSet(self, node):
        self.visit(node.variable.variable)
        self.visit(node.value)

    def visit_IndexCall(self, node):
        self.visit(node.variable)
        self.visit(node.index)

    def visit_ExpressionStatement(self, node):
        self.visit(node.expr)

    def visit_AssertStatement(self, node):
        self.visit(node.expr)

    def visit_DataObject(self, node):
        pass
