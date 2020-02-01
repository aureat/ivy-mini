"""
***  IVY Language Interpreter
***  AST Nodes
"""

""" STATEMENT COMPOUNDS """

class Program:
    def __init__(self, program):
        self.block = program

class Block:
    def __init__(self, block, scope=True):
        self.block = block
        self.scope = scope
        self.name = None

class Compound:
    def __init__(self, block):
        self.list = block

""" STRUCTURES """
class StructDeclaration:
    def __init__(self, name, super, block):
        self.name = name
        self.super = super
        self.block = block
        self.statics = []
        self.attributes = []

class StaticDeclaration:
    def __init__(self, obj, token):
        self.node = obj
        self.token = token

class MethodDeclaration:
    def __init__(self, obj):
        self.decl = obj

class ConstructNew:
    def __init__(self, newtoken, vartoken, list_expr):
        self.token = newtoken
        self.variable = vartoken
        self.list_expr = list_expr

""" FUNCTIONS """
class FunctionDeclaration:
    def __init__(self, func):
        self.name = None
        self.func = func
        self.static = False
        self.method = False

""" STATEMENT TYPES """

class Assignment:
    def __init__(self, id, expr):
        self.id = id
        self.value = expr

class ExpressionStatement:
    def __init__(self, expr):
        self.expr = expr

class ReturnStatement:
    def __init__(self, token, expr):
        self.token = token
        self.expr = expr

class Return:
    def __init__(self, expr):
        self.to_return = expr

class AssertStatement:
    def __init__(self, token, expr):
        self.token = token
        self.expr = expr

class SuperStatement:
    def __init__(self, token, id):
        self.token = token
        self.name = id

class DeleteStatement:
    def __init__(self, token, id):
        self.token = token
        self.name = id

class ImportStatement:
    def __init__(self, token, id):
        self.token = token
        self.name = id

class GlobalStatement:
    def __init__(self, token, id):
        self.token = token
        self.name = id

class OuterStatement:
    def __init__(self, token, id):
        self.token = token
        self.name = id

class ReturnValue(Exception):
    def __init__(self, value):
        self.value = value

class BreakNode(Exception): pass
class ContinueNode(Exception): pass

""" EXPRESSIONS """

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

class VariableCall:
    def __init__(self, vartoken):
        self.token = vartoken
        self.callname = vartoken.value
        self.line, self.col = vartoken.copypos()

class FunctionCall:
    def __init__(self, vartoken, lexpr, token):
        self.token = token
        self.variable = vartoken
        self.list_expr = lexpr

""" SETTERS AND GETTERS """

class AttributeCall:
    def __init__(self, vartoken, attrtoken, token):
        self.token = token
        self.variable = vartoken
        self.attribute = attrtoken

class AttributeSet:
    def __init__(self, id, att, expr):
        self.variable = id
        self.attribute = att
        self.value = expr

class IndexCall:
    def __init__(self, var, index, token):
        self.token = token
        self.variable = var
        self.index = index

class IndexSet:
    def __init__(self, var, index, value):
        self.variable = var
        self.index = index
        self.value = value


""" CONTROL FLOW """

class Conditional:
    def __init__(self, cond, ifb, elseb):
        self.condition = cond
        self.ifblock = ifb
        self.elseblock = elseb

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
