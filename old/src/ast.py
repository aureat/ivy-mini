"""
*** PARSER NODES
"""
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

class IndexCall:
    def __init__(self, var, index):
        self.variable = var
        self.index = index

class IndexSet:
    def __init__(self, var, index, value):
        self.variable = var
        self.index = index
        self.value = value

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

class Block:
    def __init__(self, block):
        self.block = block

class Return:
    def __init__(self, expr):
        self.to_return = expr

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

class AttributeAccess:
    def __init__(self, id, att):
        self.variable = id
        self.attribute = att

class AttributeSet:
    def __init__(self, id, att, expr):
        self.variable = id
        self.attribute = att
        self.value = expr

class WhileLoop:
    def __init__(self, token, condition, block):
        self.token = token
        self.condition = condition
        self.block = block

class ForLoop:
    def __init__(self, token, iterable, block):
        self.token = token
        self.iterable = iterable
        self.block = block

class BreakLoop:
    def __init__(self, token):
        self.token = token

class ContinueLoop:
    def __init__(self, token):
        self.token = token

class RemoveStatement:
    def __init__(self, token, name):
        self.token = token
        self.name = name

class ExpressionStatement:
    def __init__(self, expr):
        self.expr = expr
