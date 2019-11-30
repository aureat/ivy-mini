"""
***  IVY Language Interpreter
***  Main Class

(c) Altun Hasanli 2019

"""

"""
TO DO:
( ) Start the parser and the Interpreter
( ) Where do lexer objects belong to? Parser vs Interpreter
( ) Parser: implement factor storing negative number literals
( ) Parser: implement and not ...
( ) Type System error checking
( ) SymbolTable Scoping
( ) File importing processing
( ) Get property methods

( ) Elif conditionals without else conditionals

MORE TODO:
( ) Implement syntax error checking / handling inside the interpreter
( ) Integrate the trace collector and implement a stack trace error handler
( ) Locals and Globals
( ) Variable declaration without explicit type declaration
( ) Array indexing using the square bracket notation (inside the factor)
( ) Array element assignment (figure out the best data structure to implement arrays)
( ) Add the dot attribute functionality
( ) Expressions as array elements
( ) Environment has local and global references, internal system package implementation
( ) Add attribute-functions to objects
( ) Dynamic Package Creation
( ) Nested blocks implementation
( ) Expressions of type ---> 1 if x > 0 else 0

ADVANCED:
( ) Implement the access interface
( ) Implement the system package
( ) Determine the functional structure of this ivy implementation
( ) TraceStack vs CallStack
( ) TraceStack object inheritance by system objects

( ) Custom operations on other classes

"""

"""
LANGUAGE GRAMMAR.

package = package [identifier]
import = import [identifier]

program := ([statement] | [function-declaration] | [conditional] | [while-loop] | [for-loop])*

statement := ([declaration] | [assignment] | [expression] | [package] | [import] | break | continue) ";"
list-statements := (statement)*

assignment := [declaration] = [expression]
declaration := [type] [identifier]
function-declaration := func [identifier]: "(" [list-parameters] ")" ( -> "(" [list-parameters] ")" )? [block]

%%%%%%%%%%%%%
expression := [binfactor] (and [binfactor])*
binfactor := [binary] (or [binary])*
binary := [term] ((< | <= | > | >= | == | ===) [term])?
term := [factor] ((+|-) [factor])*
factor := [atom] ((*|/|%) [atom])*
atom := [number] | [string] | [function-call] | [attribute-call] | [index-call] | "(" [expression] ")"
%%%%%%%%%%%%%

list-expression := ([expression],)*
collection := [ list-expression ]

type := [identifier] | int | float | str | bool | coll | arr | dict | func
list-parameters := ([declaration],)*
function-block := function ( [list-parameters] ) [block]
block := { [program] }

variable-call := [identifier]
function-call := ([identifier] | [function-block]) "(" [list-expression] ")"
attribute-call := [identifier] ("." [identifier])+
index-call := ([identifier] | [collection]) "[" [expression] "]"

range := [integer] .. [integer]
iteration := [identifier] in ([range] | [collection])

conditional := if [expression] [block] (elif [expression] [block])* else [block]
while-loop := while [expression] [block]
for-loop := for [iteration] [block]

identifier := [a-zA-Z_]([a-zA-Z0-9_])*
number := (+|-| ) [integer] | [float]
integer := [0-9]+ (TokenType.INTEGER_CONSTANT)
float := [0-9]*(.[0-9]+)? (TokenType.FLOAT_CONSTANT)
boolean := true | false
string := " [.*] " | ' [.*] '

"""

import os
import sys
import argparse
from enum import Enum
import string

"""
*** SYSTEM OBJECTS
"""
from objects import IvyObject, Null, DataObject, Integer, Float, String, Boolean, TrueBoolean, FalseBoolean, Collection, Dictionary, Array, Function, Block, CodeObject

"""
*** ERROR Objects
"""

class ErrorCode(Enum):
    UNEXPECTED_TOKEN = 'Unexpected token'
    ID_NOT_FOUND = 'Identifier not found'
    STRING_NOT_CLOSED = 'String not completed'

class Error(IvyObject):
    def __init__(self, desc, name, trace=None, token=None, type=None):
        super().__init__()
        self.objdef.update({
            'error_name': name,
            'error_details': desc,
            'error_token': token,
            'error_trace': trace,
            'error_type': type})

    def get_error(self):
        error = '\nTraceback:\n'
        for i in self.attrget('error_trace').get_trace():
            line = i['line_content']
            char = i['col']
            trace_beg = min(30,len(line[:char]))
            trace_end = min(30,len(line[char:]))
            error += '   ' + str(i['filepath']) + '\n'
            error += '   File {}, line {} (col. {}) in {}\n'.format(i['filename'], i['line']+1, i['col']+1, i['name'])
            error += '\t' + line[char-trace_beg:char+trace_end] + '\n'
            if i['col'] or i['col'] == 0:
                error += '\t' + " " * i['col'] + '^\n'
                error += '{}: {}\n'.format(self.objdef['error_name'], self.objdef['error_details'])
        return error

    def __repr__(self):
        self.get_error()

    def __str__(self):
        self.get_error()

class IvySyntaxError(Error):
    def __init__(self, desc, trace=None):
        super().__init__(desc, 'SyntaxError', trace)

class IvyParseError(Error):
    def __init__(self, desc, trace=None):
        super().__init__(desc, 'ParseError', trace)

class IvyLexerError(Error):
    def __init__(self, desc, trace=None):
        super().__init__(desc, 'LexerError', trace)

class IvyIOError(Error):
    def __init__(self, desc, file=None):
        self.file = file
        super().__init__(desc, 'IOError', trace=None)

    def get_error(self, file):
        mes = self.attrget('error_details')
        fn = file.name
        return 'File ' + fn + ' with path ' + file.path + ' not found.'

    def __str__(self):
        return self.get_error(self.file)

    def __repr__(self):
        return self.__str__()

class IvyTypeError(Error):
    def __init__(self, desc, trace=None):
        super().__init__(desc, 'TypeError', trace)

"""
*** Token and TokenType Clases
"""

class TokenType(Enum):
    # SINGLE CHARACTER
    PLUS = '+'
    MINUS = '-'
    MUL = '*'
    FLOAT_DIV = '/'
    MOD = '%'
    LPAREN = '('
    RPAREN = ')'
    SEMICOLON = ';'
    DOT = '.'
    COLON = ':'
    COMMA = ','
    EQUALS = '='
    COMP_LT = '>'
    COMP_GT = '<'
    EXCLAMATION = '!'
    L_SQ_BRACK = '['
    R_SQ_BRACK = ']'
    LBRACK = '{'
    RBRACK = '}'
    S_QUOTE = "'"
    D_QUOTE = '"'
    # TWO CHARACTERS
    POWER = '**'
    COMP_EQ = '=='
    COMP_LTE = '>='
    COMP_GTE = '<='
    COMP_EQ_NOT = '!='
    FROM_TO = '->'
    COMP_IN = 'in'
    TWO_DOTS = '..'
    # THREE CHARACTERS
    ELLIPSIS = '...'
    COMP_ID = '==='
    COMP_ID_NOT = '!=='
    # IMPORTING AND MANAGEMENT
    PACKAGE = 'package'
    IMPORT = 'import'
    USE = 'use'
    GLOBAL = 'global'
    # Helper
    PRINT = 'print'
    # DATA & TYPES
    BOOLEAN = 'bool'
    INTEGER = 'int'
    FLOAT = 'float'
    STRING = 'str'
    COLLECTION = 'coll'
    ARRAY = 'arr'
    FUNC = 'func'
    FUNCTION = 'function'
    STRUCT = 'struct'
    # DATA
    TRUE = 'true'
    FALSE = 'false'
    NULL = 'null'
    # CONTROL FLOW
    IF = 'if'
    ELSE = 'else'
    ELIF = 'elif'
    RETURN = 'return'
    AND = 'and'
    OR = 'or'
    NOT = 'not'
    # LOOP
    WHILE = 'while'
    FOR = 'for'
    BREAK = 'break'
    CONTINUE = 'continue'
    # TYPE & EOF
    IDENTIFIER = 'IDENTIFIER'
    INTEGER_CONST = 'INTEGER_CONST'
    FLOAT_CONST = 'FLOAT_CONST'
    STRING_CONST = 'STRING_CONST'
    BOOLEAN_CONST = 'BOOLEAN_CONST'
    EOF = 'EOF'

class Token:
    def __init__(self, type=None, value=None, line=0, col=0):
        self.type = type
        self.value = value
        self.line = line
        self.col = col
        self.length = len(value) if value is not None else None

    def copypos(self):
        return (self.line, self.col)

    def __str__(self):
        return 'Token({type}, {value}, pos={line}:{col})'.format(type=self.type, value=str(self.value), line=self.line+1, col=self.col+1)

    def __repr__(self):
        return self.__str__()

"""
*** FILES AND PACKAGES
"""

class Package: pass

class DynamicPackage(Package): pass

class File:
    def __init__(self, name, path, content, package=None):
        self.name = name
        self.path = path
        self.content = content
        self.package = package

    def split_lines(self):
        return self.content.split('\n')

    def get_line(self, ind):
        return self.split_lines()[ind]

class Module:
    def __init__(self, name=None):
        self.name = name if name is not None else '<main>'

"""
*** Lexer Tools Generation
"""

def language_keywords():
    token_types = list(TokenType)
    ind_start = token_types.index(TokenType.PACKAGE)
    ind_end = token_types.index(TokenType.CONTINUE)
    retdict = {token_type.value: token_type for token_type in token_types[ind_start:ind_end+1]}
    return retdict

"""
*** LEXER CONSTANTS
"""
ASCII_LETTERS = string.ascii_letters
DIGITS = string.digits
ALPHA_NUMERIC = ASCII_LETTERS + DIGITS
RESERVED_KEYWORDS = language_keywords()
SINGLE_TOKENS = ['+', '/', '%', '(', ')', ';', ':', ',', '[', ']', '{', '}']
TERM_TOKENS = ['+','-','*','/','%','!','=','>','<','.',',']

"""
*** LEXER AND TOKENIZER
"""
class Lexer(object):

    def __init__(self, trace, file=None):
        self.trace = trace
        if file is not None:
            self.load(file)

    def load(self, file):
        self.file = file
        self.program = file.content
        self.pos = 0
        self.current_char = self.program[self.pos]
        self.line = 0
        self.col = 0
        self.current_token = None

    def error(self, mes, tok):
        self.trace.add_trace(self.file, tok)
        err = IvyLexerError(mes, self.trace)
        output = err.get_error()
        print(output)
        quit()

    def advance(self):
        if self.current_char == '\n':
            self.line += 1
            self.col = 0
        self.pos += 1
        if self.pos > len(self.program) - 1:
            self.current_char = None
        else:
            self.current_char = self.program[self.pos]
            self.col += 1

    def peek(self, more=0):
        peek_pos = self.pos + 1 if more==0 else self.pos + 2
        if peek_pos > len(self.program) - 1:
            return None
        else:
            return self.program[peek_pos]

    def match(self, char):
        if self.pos <= len(self.program) - 1:
            if self.current_char == char:
                self.advance()
                return True
        return False

    def skip_whitespace(self):
        while self.current_char != None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '\n':
            self.advance()

    def eat_number(self):
        token = Token(line=self.line, col=self.col)
        result = ''
        while self.current_char != None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        if self.current_char == '.' and self.peek().isdigit():
            result += self.current_char
            self.advance()
            while self.current_char != None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
            token.type = TokenType.FLOAT_CONST
            token.value = float(result)
        else:
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)
        return token

    def is_idchar(self, char):
        if char.isalpha() or char == '_':
            return True
        return False

    def eat_id(self):
        token = Token(line=self.line, col=self.col)
        value = ''
        if self.is_idchar(self.current_char):
            value += self.current_char
            self.advance()
        while self.current_char != None and self.is_idchar(self.current_char):
            value += self.current_char
            self.advance()
        token_type = RESERVED_KEYWORDS.get(value)
        if token_type is None:
            token.type = TokenType.IDENTIFIER
            token.value = value
        else:
            token.type = token_type
            token.value = value
        return token

    def eat_string(self, quote):
        value = ''
        while self.current_char != None and self.current_char != quote:
            value += self.current_char
            self.advance()
        return value

    def rtoken(self, tok, val):
        # if self.current_char != None and self.current_char in TERM_TOKENS:
        #     self.error('Unexpected token', tok)
        tok.type = TokenType(val)
        tok.value = val
        return tok

    def get_token(self):
        while self.current_char != None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            if self.current_char == '/' and self.peek() == '/':
                self.advance()
                self.advance()
                self.skip_comment()
                continue
            tok = Token(line=self.line, col=self.col)
            for q in ['\'', '"']:
                if self.match(q):
                    str_tok = Token(line=self.line,col=self.col)
                    string = self.eat_string(q)
                    str_tok.type = TokenType.STRING_CONST
                    str_tok.value = string
                    tok2 = Token(line=self.line, col=self.col)
                    if self.match(q):
                        return self.rtoken(tok, q), str_tok, self.rtoken(tok2, q)
                    else:
                        self.error('Expected a `' + q + '` to finish string literal', tok2)
            if self.is_idchar(self.current_char):
                return self.eat_id()
            elif self.current_char.isdigit():
                return self.eat_number()
            if self.match('='):
                if self.match('='):
                    if self.match('='):
                        return self.rtoken(tok, '===')
                    return self.rtoken(tok, '==')
                return self.rtoken(tok, '=')
            if self.match('-'):
                if self.match('>'):
                    return self.rtoken(tok, '->')
                return self.rtoken(tok, '-')
            if self.match('*'):
                if self.match('*'):
                    return self.rtoken(tok, '**')
                return self.rtoken(tok, '*')
            if self.match('>'):
                if self.match('='):
                    return self.rtoken(tok, '>=')
                return self.rtoken(tok, '>')
            if self.match('<'):
                if self.match('='):
                    return self.rtoken(tok, '<=')
                return self.rtoken(tok, '<')
            if self.match('!'):
                if self.match('='):
                    if self.match('='):
                        return self.rtoken(tok, '!==')
                    return self.rtoken(tok, '!=')
                return self.rtoken(tok, '!')
            if self.match('.'):
                if self.match('.'):
                    if self.match('.'):
                        return self.rtoken(tok, '...')
                    return self.rtoken(tok, '..')
                return self.rtoken(tok, '.')
            for i in SINGLE_TOKENS:
                if self.match(i):
                    return self.rtoken(tok, i)
            if self.current_char != None:
                self.error('Unexpected token', tok)
        return Token(type=TokenType.EOF, value=None, line=self.line, col=self.col)

    def tokenize(self):
        tokens = []
        tok = self.get_token()
        token = [i for i in tok] if type(tok) == tuple else [tok]
        while token[0].type!=TokenType.EOF:
            tokens += token
            tok = self.get_token()
            token = [i for i in tok] if type(tok) == tuple else [tok]
        tokens += token
        return tokens

"""
*** Trace Stack
"""
class TraceStack(IvyObject):
    def __init__(self):
        super().__init__(self)
        self.objdef.update({
            'trace': []
        })

    def add_trace(self, file, token):
        self.objdef['trace'].append({
            'file': file,
            'token': token,
            'module': Module()
        })

    def get_trace(self):
        trace = []
        for i in self.objdef['trace']:
            trace.append({
                'filename': i['file'].name,
                'filepath': i['file'].path,
                'content': i['file'].content,
                'line_content': i['file'].get_line(i['token'].line),
                'line': i['token'].line,
                'col': i['token'].col,
                'name': i['module'].name,
            })
        return trace

class SystemConstants(Enum):
    SYSTEM_BUILTINS = ['length', 'type', 'repr', 'name', 'obj', 'objdef']
    BUILTIN_NAMES = ['Integer', 'Float', 'String', 'true', 'false', 'Function']
    INTEGER = 'INTEGER'
    FLOAT = 'FLOAT'
    STRING = 'STRING'
    FUNCTION = 'FUNCTION'

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

"""
*** Parser Helper Constants
"""

TYPE_MAP = {'int': 'Integer', 'float': 'Float', 'bool': 'Boolean',
            'str': 'String', 'coll': 'Collection', 'arr': 'Array', 'func': 'Function'}

"""
*** PARSER
"""

class Parser(object):
    def __init__(self, trace=None, tokens=None, file=None):
        self.tokens = tokens
        self.toklen = len(self.tokens)
        self.trace = trace
        self.file = file
        self.idtoken = 0
        self.ctoken = self.tokens[0]

    """ TOKEN METHODS """
    def next_token(self):
        self.idtoken += 1
        self.ctoken = self.tokens[self.idtoken]

    def peek(self, amount=0):
        amount = amount if amount > 1 else 1
        return self.tokens[self.idtoken+amount]

    def peek_match(self, token_type, amount=0):
        if self.peek(amount).type == token_type:
            return True
        return False

    def match(self, token_type):
        if self.idtoken < self.toklen - 1:
            if self.ctoken.type == token_type:
                self.next_token()
                return True
        return False

    def current(self, token_type):
        if self.idtoken < self.toklen - 1:
            if self.ctoken.type == token_type:
                return True
        return False

    def eat(self, token_type):
        if self.ctoken.type == token_type:
            self.ctoken = self.next_token()

    """ Syntax Error Detection """
    def error(self, token=None, mes=None):
        mes = mes if mes is not None else 'Invalid Syntax'
        token = token if token is not None else self.ctoken
        self.trace.add_trace(self.file, token)
        err = IvySyntaxError(mes, self.trace)
        output = err.get_error()
        print(output)

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
            res= String(string)
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
                        params = None
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
            if self.match(TokenType.PRINT):
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

    """ SYNTAX EATERS """
    def eat_assignment(self):
        if self.ctoken.type == TokenType.IDENTIFIER or self.ctoken.value in TYPE_MAP.keys():
            if self.peek_match(TokenType.IDENTIFIER, 1):
                type, id = self.eat_declaration()
                if self.match(TokenType.EQUALS):
                    if self.ctoken.type == TokenType.FUNCTION:
                        expr = self.function_expression()
                        return VariableAssignment(type, id, expr)
                    return VariableAssignment(type, id, self.expression())
                return VariableDeclaration(type, id)
            elif self.peek_match(TokenType.EQUALS):
                id = self.eat_id()
                if self.match(TokenType.EQUALS):
                    if self.ctoken.type == TokenType.FUNCTION:
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
        if self.ctoken.type == TokenType.IDENTIFIER or self.ctoken.value in TYPE_MAP.keys():
            if self.peek_match(TokenType.IDENTIFIER):
                params = [(self.eat_type(), self.eat_id())]
                while self.match(TokenType.COMMA):
                    params.append((self.eat_type(), self.eat_id()))
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
        print(cond)
        print(conds)
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
        print(self.ctoken)
        if not res:
            res = self.statement()
        return res

    def parse(self):
        res = self.program()
        if self.ctoken.type == TokenType.EOF:
            return Program(res)
        self.error(mes='EOF Error')

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
    def __init__(self, parser, callstack, file=None):
        self.file = file
        self.parser = parser
        self.tree = self.parser.parse()
        self.callstack = callstack

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
            self.visit(i)

    def visit_Function(self, node):
        return node

    def visit_Conditional(self, node):
        comp = self.visit(node.condition)
        if comp:
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
            return +self.visit(node.expr)
        elif op == TokenType.MINUS:
            return -self.visit(node.expr)
        elif op == TokenType.NOT:
            return not self.visit(node.expr)

    def visit_VariableDeclaration(self, node):
        var_type = node.type.type
        var_name = node.id.value
        record = self.callstack.peek()
        record[var_name] = (var_type, Null())

    def visit_VariableAssignment(self, node):
        type = node.type.type
        id = node.id.value
        value = self.visit(node.value)
        record = self.callstack.peek()
        record[id] = (type, value)

    def visit_Assignment(self, node):
        id = node.id.value
        value = self.visit(node.value)
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
        record = self.callstack.copy(Record('func', Rec.FUNCTION, cur_depth+1))
        func = func[1]
        if len(func.params) != len(node.list_expr):
            print("Number of arguments given to funciton do not match number of declared parameters")
        for n, i in enumerate(func.params):
            val = self.visit(node.list_expr[n])
            record[i[1].value] = (val.gettype(), val)
        self.callstack.push(record)
        call = self.visit(func.block)
        self.callstack.pop()
        return call

    def visit_AttributeCall(self, node):
        return self.visit(node.variable).attrget(node.attribute.value)

    def visit_MethodCall(self, node):
        met = self.visit(node.variable).getmethod(node.method.value)
        if not isinstance(met, Boolean):
            return met()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)

"""
*** SYSTEM CONSTANTS
"""
SYSTEM_LOG_SCOPE = False
SYSTEM_LOG_CALLS = False
SYSTEM_LOG_TRACES = False

"""
*** Ivy System
"""

class SystemRuntime: pass

class System:

    def __init__(self):
        self.exception = False

        """ Python sys module """
        self.python = sys

        """ System Trace Stack """
        self._trace = TraceStack()
        self._callstack = CallStack()

        self.system = Record('system', Rec.SYSTEM, 0)

        """ Main System Objects """
        self._lexer = Lexer(self._trace)

    def initsys(self, file=None, tokens=None):
        file = file if file is not None else self.createfile('<stdin>','<stdin>','')
        self._parser = Parser(self._trace, tokens, file)
        self._interp = Interpreter(self._parser, self._callstack)

    def tokenizefile(self, path):
        file = self.createfilefrompath(path)
        self._lexer.load(file)
        return self._lexer.tokenize()

    def createfile(self, name, path, content):
        if len(content) > 0:
            return File(name, path, content)
        exit(0)

    def createfilefrompath(self, path):
        fn, fp, content = self.readfile(path)
        file = File(fn, fp, content)
        # self._trace.add_trace(file, None)
        if len(content) > 0:
            return file
        exit(0)

    def tokenized(self, path):
        self._lexer.load(self.createfilefrompath(path))
        for i in self._lexer.tokenize():
            print(i)

    def readfile(self, path):
        script_dir = os.path.dirname(os.path.abspath(__file__))
        abs_file_path = os.path.join(script_dir, path)
        if os.path.exists(abs_file_path):
            full_path = abs_file_path
        else:
            full_path = path
        try:
            self.filepath = full_path
            self.filename = full_path.split('/')[-1]
            yield self.filename
            yield full_path
            with open(full_path, 'r') as content_file:
                content = content_file.read()
                yield content
        except IOError:
            yield None
            print(IvyIOError('No such file found', File(self.filename, self.filepath, content=None)))
            quit()

    def runfile(self, path):
        file = self.createfilefrompath(path)
        tokens = self.tokenizefile(path)
        self.initsys(file, tokens)
        res = self._interp.interpret()

    def push_error(self): pass

    def dynamic_package(self):
        return

"""
*** Main Ivy Console
*** Provides access to System()
"""
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

        """ Set console info constants """
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

    def run(self, program):
        self._system.run_code(program)

    def repl(self):
        """ The REPL """
        print("Ivy Language REPL.")
        while True:
            getin = input("ivy> ")
            self.run(str(getin))

if __name__ == '__main__':
    ivy = IvyConsole()
    ivy.main()
