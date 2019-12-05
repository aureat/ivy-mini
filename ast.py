"""
*** PARSER NODES
"""
class VariableType:
    def __init__(self, token, gtype):
        self.typetoken = token
        self.type = gtype

class VariableDeclaration:
    def __init__(self, type, id):
        self.type = type
        self.id = id

class VariableAssignment:
    def __init__(self, type, id, expr):
        self.type = type
        self.id = id
        self.value = expr

class Assignment:
    def __init__(self, id, expr):
        self.id = id
        self.value = expr

class VariableCall:
    def __init__(self, vartoken):
        self.token = vartoken
        self.callname = vartoken.value
        self.line, self.col = vartoken.copypos()

class AttributeCall:
    def __init__(self, vartoken, attrtoken):
        self.variable = vartoken
        self.attribute = attrtoken

class MethodCall:
    def __init__(self, vartoken, attrtoken, params):
        self.variable = vartoken
        self.method = attrtoken
        self.params = params

class IndexCall:
    def __init__(self, vartoken, indextoken):
        self.variable = vartoken
        self.callname = vartoken.value
        self.line1, self.col1 = vartoken.copypos()
        self.index = indextoken
        self.indexname = indextoken.value
        self.line2, self.col2 = indextoken.copypos()

class FunctionCall:
    def __init__(self, vartoken, lexpr):
        self.variable = vartoken
        self.list_expr = lexpr

class ReturnStatement:
    def __init__(self, token, expr):
        self.ret = token
        self.expr = expr

class BinaryOperator:
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class UnaryOp:
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr

class UnaryOperator:
    def __init__(self, op, right):
        self.op = op
        self.right = right

class Program:
    def __init__(self, program):
        self.block = program

class PackageDeclaration:
    def __init__(self, id):
        self.id = id

class ImportStatement:
    def __init__(self, id):
        self.id = id

class Param:
    def __init__(self, type, id):
        self.type = type
        self.id = id

class CompoundStatement:
    def __init__(self, compound):
        self.compound = compound

class Conditional:
    def __init__(self, cond, ifb, elseb):
        self.condition = cond
        self.ifblock = ifb
        self.elseblock = elseb

class BreakLoop:
    def __init__(self, token):
        self.token = token

class ContinueLoop:
    def __init__(self, token):
        self.token = token

class PrintStatement:
    def __init__(self, expr):
        self.expr = expr

class Return:
    def __init__(self, expr):
        self.to_return = expr
