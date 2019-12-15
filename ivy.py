"""
***  IVY Language Interpreter
***  Main File

(c) Altun Hasanli 2019

"""

import os
import sys
import argparse
from enum import Enum

"""
*** SYSTEM OBJECTS
"""
from objects2 import *
from callstack import *
from error import *
from lexer import Lexer, RESERVED_KEYWORDS
from tokentype import TokenType
from ast import *

"""
*** Trace Stack
"""

class SystemTrace():
    def __init__(self):
        self.trace = []

    def add(self, type, framename=None, filepath=None, file=None, token=None):
        trace = {'type': type, 'name': framename, 'filepath': filepath, 'file': file, 'token': token}
        self.trace.append(trace)

    def peek(self):
        return self.trace[-1]

    def pop(self):
        self.trace.pop()

    def __repr__(self):
        return '\n'.join([str(i) for i in self.trace])

class SystemConstants(Enum):
    SYSTEM_BUILTINS = ['length', 'type', 'repr', 'name', 'obj', 'objdef']
    BUILTIN_NAMES = ['Integer', 'Float', 'String', 'Boolean', 'Function']

SYSTEM_TRACE = SystemTrace()

"""
*** Internal File Object
"""
class InternalFile:
    def __init__(self, file, tokens):
        self.file = file
        self.tokens = tokens
        self.counter = 0
        self.toklen = len(tokens)

    def next(self):
        token = self.tokens[self.counter]
        self.counter += 1
        return token

    def peek(self, amount=0):
        amount = amount if amount > 0 else 0
        return self.tokens[self.counter + amount]

"""
*** PARSER
"""

ObjMachine = ObjectMachine(SYSTEM_TRACE)

class Parser(object):
    def __init__(self, file=None, trace=None):
        self.trace = trace
        self.file = file
        self.ctoken = None

    """ Syntax Error Detection """
    def error(self, token=None, mes=None):
        mes = mes if mes is not None else 'Invalid Syntax'
        token = token if token is not None else self.ctoken
        self.trace.add('Parsing file', file=self.file.file, token=token)
        err = IvySyntaxError(mes, self.trace)
        raise err

    """ FILE and PARSER METHODS """
    def load(self, file, tokens):
        self.file = InternalFile(file, tokens)
        self.ctoken = self.file.next()

    def parse(self, file, tokens):
        self.load(file, tokens)
        res = self.program()
        if self.ctoken.type == TokenType.EOF:
            return Program(res)
        self.error(mes='EOF Error')

    def parsefile(self, file, tokens):
        self.load(file, tokens)
        return self.parse()

    """ TOKEN METHODS """
    def next_token(self):
        self.ctoken = self.file.next()

    def peek_match(self, token_type, amount=0):
        if self.file.counter < self.file.toklen:
            if self.file.tokens[self.file.counter+amount-1].type == token_type:
                return True
        return False

    def match(self, token_type):
        if self.file.counter < self.file.toklen:
            if self.ctoken.type == token_type:
                self.next_token()
                return True
        return False

    def current(self, token_type):
        if self.file.counter < self.file.toklen - 1:
            if self.ctoken.type == token_type:
                return True
        return False

    def eat(self, token_type):
        if self.ctoken.type == token_type:
            self.ctoken = self.next_token()

    def eatdef(self, token_type):
        token = self.ctoken
        if self.ctoken.type == token_type:
            self.next_token()
            return token
        else:
            self.error(mes='Unexpected token')

    """ EXPRESSIONS """

    def eat_collection(self):
        token = self.ctoken
        if self.match(TokenType.L_SQ_BRACK):
            if not self.current(TokenType.R_SQ_BRACK):
                coll = [self.expression()]
                while self.match(TokenType.COMMA):
                    coll.append(self.expression())
                if self.match(TokenType.COMMA): pass
                if self.match(TokenType.R_SQ_BRACK):
                    return ObjMachine.new(coll, token)
                self.error(mes='Expected a closing `]` to finish collection')
            elif self.match(TokenType.R_SQ_BRACK):
                return ObjMachine.new([], token)
        return False

    def atom(self):
        token = self.ctoken
        res = None
        do_attr = True
        if self.current(TokenType.L_SQ_BRACK):
            res = self.eat_collection()
        elif self.match(TokenType.PLUS):
            res = UnaryOp(token, self.atom())
        elif self.match(TokenType.MINUS):
            res=UnaryOp(token, self.atom())
        elif self.match(TokenType.TRUE):
            res = ObjMachine.fromtoken(token)
        elif self.match(TokenType.FALSE):
            res = ObjMachine.fromtoken(token)
        elif self.match(TokenType.NULL):
            res = ObjMachine.fromtoken(token)
        elif self.match(TokenType.BOOLEAN_CONST):
            res= ObjMachine.fromtoken(token)
        elif self.match(TokenType.INTEGER_CONST):
            res= ObjMachine.fromtoken(token)
        elif self.match(TokenType.FLOAT_CONST):
            res= ObjMachine.fromtoken(token)
        elif self.match(TokenType.D_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.D_QUOTE)
            res= ObjMachine.fromtoken(string)
        elif self.match(TokenType.S_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.S_QUOTE)
            res= ObjMachine.fromtoken(string)
        elif self.match(TokenType.LPAREN):
            do_attr = False
            res= self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes='Expected a closing parantheses `)` to finish expression')
        elif self.current(TokenType.FUNCTION):
            res = self.function_expression()
        else:
            if self.match(TokenType.IDENTIFIER):
                res = VariableCall(token)
            else:
                return None
        """
            Handling calls
            ex. variable.attribute[index]().attribute[][][]()().attribute
            * Index Call [ expression ]
            * Attribute Call  .identifier
            * Function Call (expression, expression, ... , expression)
        """
        if do_attr:
            while self.current(TokenType.DOT) or self.current(TokenType.LPAREN) or self.current(TokenType.L_SQ_BRACK):
                res = self.eat_call(res)
        return res

    def factor(self):
        atom = self.atom()
        ops = [TokenType.MUL, TokenType.FLOAT_DIV, TokenType.MOD, TokenType.POWER]
        while self.ctoken.type in ops:
            optok = self.ctoken
            for op in ops:
                if self.match(op):
                    atom = BinaryOperator(atom, optok, self.atom())
        return atom

    def term(self):
        token = self.ctoken
        is_not = False
        if self.match(TokenType.NOT):
            is_not = True
        factor = self.factor()
        ops = [TokenType.PLUS, TokenType.MINUS]
        while self.ctoken.type in ops:
            optok = self.ctoken
            for op in ops:
                if self.match(op):
                    factor = BinaryOperator(factor, optok, self.factor())
        if is_not:
            return UnaryOp(token, factor)
        return factor

    def binary(self):
        term = self.term()
        ops = [TokenType.COMP_LT, TokenType.COMP_LTE, TokenType.COMP_GT, TokenType.COMP_GTE,
               TokenType.COMP_EQ, TokenType.COMP_EQ_NOT, TokenType.COMP_ID, TokenType.COMP_ID_NOT]
        if self.ctoken.type in ops:
            optok = self.ctoken
            for op in ops:
                if self.match(op):
                    term = BinaryOperator(term, optok, self.term())
        return term

    def binfactor(self):
        binary = self.binary()
        ops = [TokenType.OR]
        while self.ctoken.type in ops:
            optok = self.ctoken
            for op in ops:
                if self.match(op):
                    binary = BinaryOperator(binary, optok, self.binary())
        return binary

    def expression(self):
        binfactor = self.binfactor()
        if binfactor != None:
            ops = [TokenType.AND]
            while self.ctoken.type in ops:
                optok = self.ctoken
                for op in ops:
                    if self.match(op):
                        binfactor = BinaryOperator(binfactor, optok, self.binfactor())
            return binfactor

    def function_expression(self):
        token = self.ctoken
        if self.match(TokenType.FUNCTION):
            if self.match(TokenType.LPAREN):
                list_decl = self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error('Expected a closing parantheses')
                if not self.current(TokenType.LBRACK):
                    self.error('Expected a block to define anonymous function')
                block = self.eat_block()
                return Function(params=list_decl, code=block, token=token, trace=SYSTEM_TRACE)

    """ STATEMENTS """
    def statement(self):
        token = self.ctoken
        res = self.eat_assignment()
        if not res:
            if self.match(TokenType.RETURN):
                expr = None
                if not self.peek_match(TokenType.SEMICOLON):
                    expr = self.expression()
                res = ReturnStatement(token, expr)
            elif self.match(TokenType.PACKAGE):
                id = self.eat_id()
                res = PackageDeclaration(id)
            elif self.match(TokenType.IMPORT):
                id = self.eat_id()
                res = ImportStatement(id)
            elif self.match(TokenType.BREAK):
                res = BreakLoop(token)
            elif self.match(TokenType.CONTINUE):
                res = ContinueLoop(token)
            else:
                res = self.expression()
        semicolon = self.match(TokenType.SEMICOLON)
        if not semicolon:
            self.error(mes='Expected a `;` to finish statement')
        return res

    def compound_statement(self):
        statements = []
        while self.idtoken < self.toklen - 1:
            statements.append(self.statement())
        return CompoundStatement(statements)

    """ SYNTAX 'EATERS' """
    def eat_assignment(self):
        if self.current(TokenType.IDENTIFIER) and (self.peek_match(TokenType.EQUALS, 1) or self.peek_match(TokenType.DOT, 1)):
            id = self.eat_id()
            if self.match(TokenType.EQUALS):
                expr = self.expression()
                return Assignment(id, expr)
            id = VariableCall(id)
            while self.match(TokenType.DOT):
                att = self.eat_id()
                id = AttributeAccess(id, att)
            if self.match(TokenType.EQUALS):
                expr = self.expression()
                return AttributeSet(id, att, expr)
        return False

    def eat_list_expression(self):
        getexpr = self.expression()
        expr = [getexpr] if getexpr is not None else []
        while self.match(TokenType.COMMA):
            expr.append(self.expression())
        return expr

    def eat_call(self, expr):
        token = self.ctoken
        if self.match(TokenType.DOT):
            attr = self.ctoken
            if self.match(TokenType.IDENTIFIER):
                return AttributeCall(expr, attr)
            self.error(mes='Expected an identifier to finish attribute call')
        elif self.match(TokenType.LPAREN):
            list_expr = self.eat_list_expression()
            if self.match(TokenType.RPAREN):
                return FunctionCall(expr, list_expr)
            self.error(mes="Expected a closing paranthesis ')' to finish function call")
        elif self.match(TokenType.L_SQ_BRACK):
            index = self.expression()
            if self.match(TokenType.R_SQ_BRACK):
                return IndexCall(expr, index)
            self.error(mes="Expected a closing bracket ']' to finish function call")
        return False

    def eat_type(self):
        token = self.ctoken
        if self.ctoken.type == TokenType.IDENTIFIER or token.value in TYPE_MAP.keys():
            self.next_token()
            gtype = TYPE_MAP.get(token.value)
            if not gtype:
                gtype = token.value
            return VariableType(token, gtype)
        self.error(token, 'Invalid type')

    def eat_list_decl(self):
        params = []
        if self.current(TokenType.IDENTIFIER):
            params = [self.eat_id()]
            while self.match(TokenType.COMMA):
                params.append(self.eat_id())
            if self.match(TokenType.COMMA): pass
        return params

    def eat_declaration(self):
        type = self.eat_type()
        id = self.eat_id()
        return type, id

    def eat_id(self):
        token = self.ctoken
        if self.match(TokenType.IDENTIFIER):
            if token.value in RESERVED_KEYWORDS:
                self.error(token, 'Cannot use identifier name')
            return token
        self.error(token)

    def eat_funcblock(self):
        if self.match(TokenType.FUNCTION):
            if self.match(TokenType.LPAREN):
                params = self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error(mes='Expected a closing parantheses')
                block = self.eat_block()
                return Function(params=params, code=block)

    def eat_block(self):
        if self.match(TokenType.LBRACK):
            block = self.program(TokenType.RBRACK)
            if not self.match(TokenType.RBRACK):
                self.error(mes='Expected a closing bracket to finish block')
            return Block(block)

    def eat_conditional(self):
        conds = []
        if_elif = False
        no_else = True
        token = self.ctoken
        cond = None
        if self.match(TokenType.IF):
            if_elif = True
            if not self.match(TokenType.LPAREN):
                self.error(mes='Expected a paranthesis after conditional token')
            expr = self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes="Expected a closing paranthesis ')' to finish conditional expression")
            block = self.eat_block()
            conds.append((expr, block))
        while self.match(TokenType.ELIF):
            if not if_elif:
                self.error(token, mes='Unexpected token for conditional')
            if not self.match(TokenType.LPAREN):
                self.error(mes='Expected a paranthesis after conditional token')
            expr = self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes="Expected a closing paranthesis ')' to finish conditional expression")
            block = self.eat_block()
            conds.append((expr, block))
        if self.match(TokenType.ELSE):
            if not if_elif:
                self.error(token, mes='Unexpected token for conditional')
            block = self.eat_block()
            conds.append((None, block))
            no_else = False
        if len(conds) == 0:
            return False
        if len(conds) == 1:
            return Conditional(expr, block, None)
        if no_else:
            cond = None
        else:
            cond = conds[-1][1]
        for i in range(len(conds)-2,0,-1):
            cond = Conditional(conds[i][0], conds[i][1], cond)
        return cond

    """ PROGRAM """
    def program(self, endtok=TokenType.EOF):
        prog = [self.eat_program()]
        while self.ctoken.type != endtok:
            prog.append(self.eat_program())
        return prog

    def eat_program(self):
        res = self.eat_conditional()
        if not res:
            res = self.statement()
        return res

class SemanticAnalyzer:
    def __init__(self, scope):
        self.scope = scope

    def visit_VariableDeclaration(self, node):
        var_type = node.type.type
        var_name = node.id.value
        if self.scope.lookup(var_name, current_only=True):
            print("Such a name already exists")
        type_symbol = self.scope.lookup(var_type)
        var_symbol = Symbol(type_symbol, var_name)
        self.scope.insert(var_symbol)

"""
*** Main Interpreter
"""

class Interpreter:
    def __init__(self, callstack, trace):
        self.callstack = callstack
        self.trace = trace

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

    def builtins(self):
        record = self.callstack.peek()
        record['type'] = FunctionType(SYSTEM_TRACE)
        record['clock'] = FunctionClock(SYSTEM_TRACE)
        record['istrue'] = FunctionIstrue(SYSTEM_TRACE)
        record['repr'] = FunctionRepr(SYSTEM_TRACE)
        record['print'] = FunctionPrint(SYSTEM_TRACE)
        record['length'] = FunctionLength(SYSTEM_TRACE)

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
        visitor = getattr(self, method_name, self.notfound)
        return visitor(node)

    def notfound(self, node):
        self.error(mes="Cannot interpret ivy code", etype=IvyInterpretationError)

    def visit_Program(self, node):
        program = Record('<main>', RecordType.PROGRAM, 1)
        self.callstack.push(program)
        self.builtins()
        for i in node.block:
            self.visit(i)
        self.callstack.pop()

    def visit_Block(self, node):
        for i in node.block:
            exc = self.visit(i)
            if isinstance(exc, Return):
                return exc.to_return

    def visit_Function(self, node):
        return node

    def visit_Conditional(self, node):
        comp = self.visit(node.condition)
        if comp.istrue():
            for i in node.ifblock.block:
                exc = self.visit(i)
                if isinstance(exc, Return):
                    return exc
        else:
            if node.elseblock is not None:
                for i in node.elseblock.block:
                    exc = self.visit(i)
                    if isinstance(exc, Return):
                        return exc

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
        value = func.call(self, node)
        return value

    def visit_ReturnStatement(self, node):
        if self.callstack.peek().type != RecordType.FUNCTION:
            self.error(mes='Return statement should be inside of a function', token=node.ret)
        to_return = self.visit(node.expr) if node.expr != None else Null()
        return Return(to_return)

    def visit_AttributeCall(self, node, access=False):
        var = self.visit(node.variable)
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

"""
*** SYSTEM CONSTANTS
"""
SYSTEM_LOG_SCOPE = False
SYSTEM_LOG_CALLS = False
SYSTEM_LOG_TRACES = False

"""
*** FILES AND PACKAGES
"""

class Package: pass

class DynamicPackage(Package): pass

class Module: pass

"""
*** Main Ivy Console
*** Provides access to System()
"""

class IOModule(Module):
    def __init__(self): pass

    def error(self, mes):
        raise IvyIOError(mes, SYSTEM_TRACE)

    def filefrompath(self, path):
        tryfullpath = os.path.join(os.path.dirname(os.path.abspath(__file__)), path)
        fullpath = path
        if os.path.exists(tryfullpath):
            fullpath = tryfullpath
        try:
            filepath = fullpath
            filename = fullpath.split('/')[-1]
            return IOWrapper(filename, filepath)
        except IOError:
            self.error('Cannot find file from specified path')

    def __repr__(self):
        return '<System.IO>'

    __str__ = __repr__

class IOWrapper:
    def __init__(self, name, path):
        with open(path, 'r') as file:
            self.wrapper = file
            self.contents = file.read()
        self.name = name
        self.path = path
        self.line_counter = 0

    def __repr__(self):
        return f'<System.IO.IOWrapper name={self.name} path={self.path}>'

    __str__ = __repr__

    def split(self):
        return self.contents.split('\n')

    def getline(self, ind):
        return self.split()[ind]

    def readline(self):
        content = self.split()[self.line_counter]
        self.line_counter += 1
        return content

    def read(self):
        return self.contents

    def write(self, line):
        with open(path, 'w') as file:
            self.wrapper = file
            file.write(line)

"""
*** Ivy System
"""

class System:
    def __init__(self):
        "IO Module for System"
        self.io = IOModule()

        """ System Memory """
        self.callstack = CallStack()

        """ System Modification for the call stack """
        self.system = Record('<system>', RecordType.SYSTEM, 0)
        self.callstack.push(self.system)

        """ Initialize System Objects """
        self.lexer = Lexer(trace=SYSTEM_TRACE)
        self.parser = Parser(trace=SYSTEM_TRACE)
        self.interpreter = Interpreter(callstack=self.callstack, trace=SYSTEM_TRACE)

    """ LEXER METHODS """

    def tokenizefile(self, path):
        file = self.io.filefrompath(path)
        return file, self.lexer.tokenizefile(file)

    """ RUN METHODS """

    def tokenized(self, path):
        try:
            for i in self.tokenizefile(path)[1]: print(i)
        except Error as e:
            print(e)

    def runfile(self, path):
        try:
            file, tokens = self.tokenizefile(path)
            tree = self.parser.parse(file, tokens)
            res = self.interpreter.interpret(file, tree)
        except Error as e:
            print(e)

    def repl(self):
        """ The REPL """
        sys.stderr.write("Ivy Language REPL.")
        while True:
            try:
                getin = input("ivy> ")
                self.run_code(str(getin))
            except Error as e:
                print(e)

class IvyConsole:
    def __init__(self):
        self._system = System()

    def main(self):
        """ Define the arguments for the console """
        parser = argparse.ArgumentParser(description='Ivy Language Console')
        parser.add_argument('-f', '--file', default=False, help='Execute Ivy Source File')
        parser.add_argument('-p', '--repl', default=False, action='store_true', help='Ivy Language Read-Eval-Print loop')
        parser.add_argument('-t', '--tokenize', default=False, help='Tokenize a source file')
        parser.add_argument('--traces', action='store_true', help='Print the trace stack of the program')
        parser.add_argument('--calls', action='store_true', help='Print the call stack of the program')
        parser.add_argument('--scope', action='store_true', help='Print scope info about the program')
        args = parser.parse_args()

        """ Set console information """
        global SYSTEM_LOG_SCOPE, SYSTEM_LOG_CALLS, SYSTEM_LOG_TRACES
        SYSTEM_LOG_SCOPE, SYSTEM_LOG_CALLS, SYSTEM_LOG_TRACES = args.scope, args.calls, args.traces

        """ Execute system commands """
        if len(sys.argv) == 1:
            return self.repl()
        if args.file:
            return self._system.runfile(args.file)
        if args.tokenize:
            return self._system.tokenized(args.tokenize)
        if args.repl:
            return self.repl()
        parser.print_help(sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    ivy = IvyConsole()
    ivy.main()
