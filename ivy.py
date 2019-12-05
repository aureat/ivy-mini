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
from objects import *
from error import Error, IvySyntaxError, IvyParseError, IvyLexerError, IvyIOError, IvyTypeError
from lexer import Lexer
from tokentype import TokenType
from ast import *

"""
*** Trace Stack
"""

class Trace(Enum):
    READFILE = 'Reading file'
    TOKENIZEFILE = 'Tokenizing file'
    PARSEFILE = 'Parsing file'
    INTERPRETFILE = 'Interpreting file'
    CALLFUNC = 'Calling function'
    RETURNFUNC = 'Returning function'
    CREATEOBJ = 'Creating Ivy Object'
    ENTERSCOPE = 'Entering scope'
    CLOSESCOPE = 'Closing scope'
    LOOKUPSYM = 'Looking up a symbol'
    STORESYM = 'Storing a symbol'

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
class Parser(object):
    def __init__(self, file=None, trace=None):
        self.trace = trace
        self.file = file
        self.ctoken = None

    """ Syntax Error Detection """
    def error(self, token=None, mes=None):
        mes = mes if mes is not None else 'Invalid Syntax'
        token = token if token is not None else self.ctoken
        self.trace.add(Trace.PARSEFILE, file=self.file.file, token=token)
        err = IvySyntaxError(mes, self.trace)
        raise err

    """ FILE and PARSER METHODS """
    def load(self, file, tokens):
        self.file = InternalFile(file, tokens)
        self.ctoken = self.file.next()

    def parse(self):
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

    def peek(self, amount=0):
        amount = amount if amount > 1 else 1
        return self.file.tokens[self.counter+amount]

    def peek_match(self, token_type, amount=0):
        if self.peek(amount).type == token_type:
            return True
        return False

    def match(self, token_type):
        if self.file.counter < self.file.toklen - 1:
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
    def atom(self):
        token = self.ctoken
        res = None
        do_attr = False
        if self.match(TokenType.PLUS):
            res = UnaryOp(token, self.atom())
        elif self.match(TokenType.MINUS):
            res=UnaryOp(token, self.atom())
        elif self.match(TokenType.TRUE):
            res = TrueBoolean()
        elif self.match(TokenType.FALSE):
            res = FalseBoolean()
        elif self.match(TokenType.NULL):
            res = Null()
        elif self.match(TokenType.BOOLEAN_CONST):
            do_attr = True
            res= Boolean(token.value)
        elif self.match(TokenType.INTEGER_CONST):
            do_attr = True
            res= Integer(token.value)
        elif self.match(TokenType.FLOAT_CONST):
            do_attr = True
            res= Float(token.value)
        elif self.match(TokenType.D_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.D_QUOTE)
            res= String(string.value)
        elif self.match(TokenType.S_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.S_QUOTE)
            res= String(string.value)
        elif self.match(TokenType.LPAREN):
            res= self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes='Expected a closing parantheses `)` to finish expression')
        else:
            do_attr=True
            res = self.eat_call()
            if res == False:
                self.error(token)
        while do_attr and self.ctoken.type == TokenType.DOT:
            dot = self.ctoken
            if self.match(TokenType.DOT):
                attr = self.ctoken
                if self.match(TokenType.IDENTIFIER):
                    if self.match(TokenType.LPAREN):
                        params = self.eat_list_expression()
                        if self.match(TokenType.RPAREN):
                            res = MethodCall(res, attr, params)
                            continue
                    res = AttributeCall(res, attr)
                    continue
                self.error(dot)
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
        return False

    def function_expression(self):
        if self.match(TokenType.FUNCTION):
            if self.match(TokenType.LPAREN):
                list_decl = self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error('Expected a closing parantheses')
                if not self.current(TokenType.LBRACK):
                    self.error('Expected a block to define anonymous function')
                block = self.eat_block()
                return Function(params=list_decl, code=block)

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
            elif self.match(TokenType.PRINT):
                expr = self.expression()
                res = PrintStatement(expr)
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
                res = PrintStatement(self.expression())
        semicolon = self.match(TokenType.SEMICOLON)
        if not semicolon:
            self.error(mes='Expected a `;` to finish statement')
        return res

    def compound_statement(self):
        statements = []
        while self.idtoken < self.toklen - 1:
            statements.append(self.statement())
        return CompoundStatement(statements)

    """ SYNTAX EATERS """
    def eat_assignment(self):
        if self.current(TokenType.IDENTIFIER) and self.peek_match(TokenType.EQUALS):
            id = self.eat_id()
            if self.match(TokenType.EQUALS):
                if self.current(TokenType.FUNCTION):
                    expr = self.function_expression()
                    return Assignment(id, expr)
                expr = self.expression()
                return Assignment(id, expr)
        return False

    def eat_list_expression(self):
        expr = [self.expression()]
        while self.match(TokenType.COMMA):
            expr.append(self.expression())
        return expr

    def eat_call(self):
        token = self.ctoken # Identifier
        if self.match(TokenType.IDENTIFIER):
            if self.match(TokenType.LPAREN):
                expr = self.eat_list_expression()
                if self.match(TokenType.RPAREN):
                    return FunctionCall(token, expr)
                self.error(mes='Expected a closing paranthesis to finish function call')
            elif self.match(TokenType.L_SQ_BRACK):
                expr = self.expression()
                if self.match(TokenType.R_SQ_BRACK):
                    return IndexCall(token, expr)
                self.error(mes='Expected a closing bracket to finish index call')
            else:
                return VariableCall(token)
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
        if self.match(TokenType.IF):
            if_elif = True
            expr = self.expression()
            block = self.eat_block()
            conds.append((expr, block))
        while self.match(TokenType.ELIF):
            if not if_elif:
                self.error(token, mes='Unexpected token for conditional')
            expr = self.expression()
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

"""
*** Call Stack
"""

class Rec(Enum):
    SYSTEM = 'SYSTEM'
    PROGRAM   = 'PROGRAM'
    FUNCTION   = 'FUNCTION'
    METHOD   = 'METHOD'

class CallStack:
    def __init__(self):
        self.records = []

    def copy(self, ar):
        if len(self.records) > 0:
            ar.members = self.peek().members
        return ar

    def push(self, ar):
        ar.init_builtin_frame()
        self.records.append(ar)

    def pop(self):
        return self.records.pop()

    def peek(self):
        return self.records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self.records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()

class Record:
    def __init__(self, name, type, depth):
        self.name = name
        self.type = type
        self.depth = depth
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members.get(key, False)

    def get(self, key):
        return self.members.get(key)

    def init_builtin_frame(self):
        self.members.update({
            'SYSTEM_RECORD_NAME': ('String', self.name),
            'SYSTEM_RECORD_TYPE': ('String', self.type),
            'SYSTEM_RECORD_DEPTH': ('String', self.depth),
        })

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.depth,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f'   {name:<20}: {val}')

        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()

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
    def __init__(self, callstack):
        self.file = None
        self.callstack = callstack

    def load(self, file, tree):
        self.file = file
        self.tree = tree

    def interpret(self):
        tree = self.tree
        if tree is None:
            return str()
        return self.visit(tree)

    def visit(self, node):
        if isinstance(node, DataObject) or isinstance(node, Null):
            method_name = 'visit_DataObject'
        else:
            method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.notfound)
        return visitor(node)

    def notfound(self, node):
        print("No such interpreter node found: " + type(node).__name__)

    def visit_PrintStatement(self, node):
        expr = self.visit(node.expr)
        print(expr.printable() if isinstance(expr, IvyObject) else expr)
        return expr

    def visit_Program(self, node):
        program = Record('<main>', Rec.PROGRAM, 1)
        self.callstack.push(program)
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
            return self.visit(node.ifblock)
        else:
            if node.elseblock is not None:
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
            print("detecte adn")
            print(left)
            return left.getop('and', right)
        elif node.op.type == TokenType.OR:
            return left.getop('or', right)
        elif node.op.type == TokenType.COMP_ID:
            return left.op_ideq(right)
        elif node.op.type == TokenType.COMP_ID_NOT:
            return left.op_ideq_not(right)

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

    def visit_VariableDeclaration(self, node):
        var_type = node.type.type
        var_name = node.id.value
        record = self.callstack.peek()
        record[var_name] = (var_type, Null())

    def visit_VariableAssignment(self, node):
        type = node.type.type
        id = node.id.value
        value = self.visit(node.value)
        if isinstance(value, Function):
            value.name = id
        record = self.callstack.peek()
        record[id] = (type, value)

    def visit_Assignment(self, node):
        id = node.id.value
        value = self.visit(node.value)
        if isinstance(value, Function):
            value.name = id
        record = self.callstack.peek()
        if record[id] != False:
            record[id] = (record[id][0], value)
        print("Variable not declared")

    def visit_VariableCall(self, node):
        var_name = node.callname
        record = self.callstack.peek()
        value = record[var_name]
        if value != False:
            return value[1]
        print("Variable does not exist")

    def visit_FunctionCall(self, node):
        cur_record = self.callstack.peek()
        cur_depth = cur_record.depth
        func = cur_record[node.variable.value]
        if not func:
            print("function not found")
        func = func[1]
        record = self.callstack.copy(Record(func.name, Rec.FUNCTION, cur_depth+1))
        if len(func.params) != len(node.list_expr):
            print("Number of arguments given to funciton do not match number of declared parameters")
        for n, i in enumerate(func.params):
            val = self.visit(node.list_expr[n])
            record[i[1].value] = (val.gettype(), val)
        self.callstack.push(record)
        call = self.visit(func.block)
        self.callstack.pop()
        if not call:
            return Null()
        return call

    def visit_ReturnStatement(self, node):
        if self.callstack.peek().type != Rec.FUNCTION:
            print("Return statement should be inside of a function")
        to_return = self.visit(node.expr) if node.expr != None else Null()
        return Return(to_return)

    def visit_AttributeCall(self, node):
        return self.visit(node.variable).attrget(node.attribute.value)

    def visit_MethodCall(self, node):
        metval = self.visit(node.variable)
        met = metval.getmethod(node.method.value)
        params = ','.join([str(i.data) for i in node.params]) if node.params != [False] else ''
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
            SYSTEM_TRACE.add(Trace.READFILE, filepath=fullpath) # Logging
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
        return self.split_lines()[ind]

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
        self.system = Record('<system>', Rec.SYSTEM, 0)
        self.callstack.push(self.system)

        """ Initialize System Objects """
        self.lexer = Lexer(SYSTEM_TRACE)
        self.parser = Parser(SYSTEM_TRACE)
        self.interpreter = Interpreter(self.callstack)

    def tokenizefile(self, path):
        file = self.io.filefrompath(path)
        return self.lexer.tokenizefile(file)

    def tokenized(self, path):
        for i in self.tokenizefile(path):
            print(i)
        print(SYSTEM_TRACE)

    def runfile(self, path):
        try:
            file = self.createfilefrompath(path)
            tokens = self.tokenizefile(path)
            self.initsys(file, tokens)
            res = self._interp.interpret()
        except Error as e:
            print(e.get_error())
            exit(0)

    def repl(self):
        """ The REPL """
        sys.stderr.write("Ivy Language REPL.")
        while True:
            try:
                getin = input("ivy> ")
                self.run_code(str(getin))
            except Error as e:
                print(e.get_error())
                exit(0)

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
