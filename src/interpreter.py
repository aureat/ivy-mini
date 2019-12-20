"""
*** Main Interpreter
"""

from src.ast import *
from src.callstack import *
from src.builtinfunction import *
from src.error import *

class Interpreter:
    def __init__(self, callstack, trace):
        self.callstack = callstack
        self.trace = trace
        self.main = Record('<main>', RecordType.GLOBAL, 1)
        self.callstack.push(self.main)
        self.init_env()

    """ INTERPRETER METHODS """
    def load(self, file, tree):
        self.file = file
        self.tree = tree
        self.trace.add(type='Interpreting File', file=file)

    def interpret(self, file, tree):
        self.load(file, tree)
        tree = self.tree
        if tree is None:
            return str()
        return self.visit(tree)

    def add_env(self, name, value):
        record = self.callstack.peek()
        record[name] = value

    def init_env(self):
        record = self.callstack.peek()
        record['type'] = FunctionType(self.trace, self)
        record['clock'] = FunctionClock(self.trace, self)
        record['istrue'] = FunctionIstrue(self.trace, self)
        record['isnull'] = FunctionIsnull(self.trace, self)
        record['istype'] = FunctionIstype(self.trace, self)
        record['repr'] = FunctionRepr(self.trace, self)
        record['printable'] = FunctionPrintable(self.trace, self)
        record['print'] = FunctionPrint(self.trace, self)
        record['length'] = FunctionLength(self.trace, self)
        record['callable'] = FunctionCallable(self.trace, self)
        record['indexable'] = FunctionIndexable(self.trace, self)
        #record['call'] = FunctionCallFn(self.trace)
        record['attrget'] = FunctionAttrget(self.trace, self)
        record['attrset'] = FunctionAttrset(self.trace, self)
        record['attrhas'] = FunctionAttrhas(self.trace, self)
        record['attrdel'] = FunctionAttrdel(self.trace, self)
        record['exit'] = FunctionExit(self.trace, self)

    """ ERROR HANDLING """

    def error(self, mes, token=None, etype=IvyRuntimeError):
        self.trace.add(type='Runtime', token=token, framename=self.callstack.peek().name, file=self.file)
        raise etype(mes, self.trace)

    """ VISITOR NODES """

    def visit(self, node):
        if isinstance(node, DataObject) or isinstance(node, Null):
            method_name = 'visit_DataObject'
        else:
            method_name = 'visit_' + type(node).__name__
        # print("** finding node : " + type(node).__name__)
        visitor = getattr(self, method_name, self.notfound)
        return visitor(node)

    def notfound(self, node):
        self.error(mes="Cannot interpret ivy code", etype=IvyRuntimeError)

    def visit_Program(self, node):
        self.visit(node.block)
        self.trace.clear()

    def visit_Block(self, node, btype='main'):
        for i in node.block:
            exc = self.visit(i)
            if isinstance(exc, Return):
                return exc
            if isinstance(exc, BreakLoop):
                if btype=='loop':
                    return exc
                self.error(mes='Break statement should be inside of a loop', token=exc.token)
            if isinstance(exc, ContinueLoop):
                if btype=='loop':
                    return exc
                self.error(mes='Continue statement should be inside of a loop', token=exc.token)

    def visit_Function(self, node):
        node.interpreter = self
        return node

    def visit_Conditional(self, node):
        comp = self.visit(node.condition)
        if comp.istrue():
            return self.visit(node.ifblock)
        elif node.elseblock is not None:
            return self.visit(node.elseblock)

    def visit_CompoundStatement(self, node):
        for stmt in node.compound:
            exec_stmt = self.visit(stmt)

    def visit_BinaryOperator(self, node):
        left = self.visit(node.left)
        right = self.visit(node.right)
        if node.op.type == TokenType.PLUS:
            return left.getop('add', right)
        elif node.op.type == TokenType.MINUS:
            return left.getop('sub', right)
        elif node.op.type == TokenType.MUL:
            return left.getop('mult', right)
        elif node.op.type == TokenType.FLOAT_DIV:
            return left.getop('div', right)
        elif node.op.type == TokenType.MOD:
            return left.getop('mod', right)
        elif node.op.type == TokenType.POWER:
            return left.getop('pow', right)
        elif node.op.type == TokenType.COMP_EQ:
            return left.getop('eq', right)
        elif node.op.type == TokenType.COMP_EQ_NOT:
            return left.getop('eq_not', right)
        elif node.op.type == TokenType.COMP_GT:
            return left.getop('gt', right)
        elif node.op.type == TokenType.COMP_GTE:
            return left.getop('gte', right)
        elif node.op.type == TokenType.COMP_LT:
            return left.getop('lt', right)
        elif node.op.type == TokenType.COMP_LTE:
            return left.getop('lte', right)
        elif node.op.type == TokenType.AND:
            return left.getop('and', right)
        elif node.op.type == TokenType.OR:
            return left.getop('or', right)
        elif node.op.type == TokenType.COMP_ID:
            return left.getop('ideq', right)
        elif node.op.type == TokenType.COMP_ID_NOT:
            return left.getop('ideq_not', right)

    def visit_DataObject(self, node):
        return node

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == TokenType.PLUS:
            return self.visit(node.expr).op_plus()
        elif op == TokenType.MINUS:
            return self.visit(node.expr).op_minus()
        elif op == TokenType.NOT:
            return self.visit(node.expr).op_not()

    def visit_Assignment(self, node):
        id = node.id.value
        record = self.callstack.peek()
        value = self.visit(node.value)
        if isinstance(value, Function):
            value.anonymous = False
            value.reference = id
        record[id] = value

    def visit_VariableCall(self, node):
        var_name = node.callname
        record = self.callstack.peek()
        value = record[var_name]
        if value != False:
            return value
        self.error(mes='Name ' + var_name + ' does not exist', token=node.token, etype=IvyNameError)

    def visit_FunctionCall(self, node):
        func = self.visit(node.variable)
        value = func.call(node)
        return value

    def visit_WhileLoop(self, node):
        comp = self.visit(node.condition)
        while comp.istrue():
            loop = None
            exc = self.visit(node.block, 'loop')
            if isinstance(exc, BreakLoop): break
            if isinstance(exc, ContinueLoop): continue
            if isinstance(exc, Return): return exc
            comp = self.visit(node.condition)

    def visit_BreakLoop(self, node):
        return node

    def visit_ContinueLoop(self, node):
        return node

    def call(self, func, token, params):
        func.interpreter = self
        node = FunctionCall(token, params)
        value = func.call(node)
        return value

    def visit_ReturnStatement(self, node):
        if self.callstack.peek().type != RecordType.FUNCTION:
            self.error(mes='Return statement should be inside of a function', token=node.ret)
        to_return = self.visit(node.expr) if node.expr != None else Null()
        return Return(to_return)

    def visit_RemoveStatement(self, node):
        if node.name == None:
            self.error(mes='Remove statement should be followed by a name', token=node.token)
        rec = self.callstack.peek()
        name = node.name.value
        if name in rec.members:
            rec.remove(name)
        else: self.error(mes='Name ' + name + ' does not exist', token=node.name)

    def visit_AttributeCall(self, node, access=False):
        var = self.visit(node.variable)
        if not hasattr(var, 'attrget'):
            self.error(mes="Object of type '" + var.__class__.__name__ + "' does not have the attribute 'attrget'",
                       token=node.attribute,
                       etype=IvyAttributeError)
        attr = var.attrget(node.attribute.value)
        if attr:
            return attr
        self.error(mes="Object of type '" + var.type + "' does not have the attribute '" + node.attribute.value + "'",
                   token=node.attribute,
                   etype=IvyAttributeError)

    def visit_AttributeAccess(self, node):
        return self.visit_AttributeCall(node)

    def visit_AttributeSet(self, node):
        att = node.attribute
        var = self.visit(node.variable.variable)
        value = self.visit(node.value)
        if isinstance(value, Function):
            value.anonymous = False
            value.reference = att.value
        if hasattr(var, 'attrset'):
            var.attrset(att.value, value)
            return
        self.error(mes="Object of type '" + var.type + "' does not have the attribute 'attrset'",
                   token=att,
                   etype=IvyAttributeError)

    def visit_IndexCall(self, node):
        var = self.visit(node.variable)
        index = self.visit(node.index)
        if not hasattr(var, "getitem"):
            self.error(mes='Object of type {} cannot be called by index'.format(var.type), etype=IvyIndexError, token=index.token)
        return var.getitem(index)

    def visit_ExpressionStatement(self, node):
        print(self.visit(node.expr))

    def visit_MethodCall(self, node):

        def process_param(param):
            if isinstance(param, String):
                return '"' + str(param.data) + '"'
            elif isinstance(param, Boolean):
                return str(param.__bool__())
            return str(param.data)

        metval = self.visit(node.variable)
        met = metval.getmethod(node.method.value)
        params = ','.join([process_param(i) for i in node.params]) if node.params != [False] else ''
        if not isinstance(met, Boolean):
            return eval('met({})'.format(params))
