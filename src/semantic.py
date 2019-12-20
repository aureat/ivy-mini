"""
*** IVY LANGUAGE Interpreter
    Variable Resolution & Semantic Analysis
"""

class Symbol:
    def __init__(self, name, type=None):
        self.name = name
        self.type = type

class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name={name}, type={type})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__

class FuncSymbol(Symbol):
    def __init__(self, name, params=None):
        super().__init__(name)
        self.params = params if params is not None else []

    def __str__(self):
        return '<{class_name}(name={name}, params={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__

class BuiltinName(VarSymbol):
    def __init__(self, name, type):
        super().__init__(name, type)

class BuiltinFunction(FuncSymbol):
    def __init__(self, name, params):
        super().__init__(name, params)

class SymbolTable:
    def __init__(self, name, depth, parent=None):
        self.symbols = {}
        self.name = name
        self.depth = depth
        self.parent = parent
        if parent is None:
            self.builtins()

    def builtins(self):
        self.insert(BuiltinName('Integer'))
        self.insert(BuiltinName('String'))
        self.insert(BuiltinName('Float'))
        self.insert(BuiltinName('Boolean'))
        self.insert(BuiltinName('Function'))

    __repr__ = __str__

    def insert(self, symbol):
        self.symbols[symbol.name] = symbol

    def lookup(self, name, current_only=False):
        symbol = self.symbols.get(name)
        if symbol is not None:
            return symbol
        if current_only:
            return None
        if self.parent is not None:
            return self.parent.lookup(name)

class SemanticAnalyzer:
    def __init__(self, trace, scope):
        self.trace = trace

    def process(self, file, system_scope):
        self.file = file
        self.tree = file.tree
        self.scope = SymbolTable('global', 0, system_scope)

    def visit(self, node):
        method = getattr(self, node.__class__.__name__, False)
        if not method: pass
        return method(node)

    def error(self, error_type, mes, token):

        error_type(mes, )

    def Program(self, node):
        self.visit(node.block)

    def Block(self, node):
        self.scope = SymbolTable('global', 0, self.scope)
        for i in node.block:
            self.visit(i)
        self.scope = self.scope.scope

    def BinaryOperator(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def UnaryOp(self, node):
        self.visit(node.expr)

    def PrintStatement(self, node):
        self.visit(node.expr)

    def AttributeCall(self, node):
        self.visit(node.variable)

    def Assignment(self, node):
        self.visit(node.value)

    def VariableCall(self, node):
        var_name = node.callname
        var_symbol self.scope.lookup(var_name)
        if var_symbol is None:
            self.error(IvyNameError, 'Variable not found', node.token)

    def visit_VariableDeclaration(self, node):
        var_type = node.type.type
        var_name = node.id.value
        if self.scope.lookup(var_name, current_only=True):
            print("Such a name already exists")
        type_symbol = self.scope.lookup(var_type)
        var_symbol = Symbol(type_symbol, var_name)
        self.scope.insert(var_symbol)
