"""
***  IVY Language Interpreter
***  Main Class

TO DO:
( ) Start the parser and the Interpreter
( ) Where do lexer objects belong to? Parser vs Interpreter
( ) Parser: implement factor storing negative number literals
( ) Parser: implement and not ...
( ) Type System error checking

MORE TODO:
( ) Implement syntax error checking / handling inside the interpreter
( ) Integrate the trace collector and implement a stack trace error handler
( ) Locals and Globals
( ) Variable declaration without explicit type declaration
( ) Implement and/or/xor
( ) String operations / Concatenations
( ) Array indexing using the square bracket notation (inside the factor)
( ) Array element assignment (figure out the best data structure to implement arrays)
( ) Add the dot attribute functionality
( ) Expressions as array elements
( ) Open close paranthesis counter
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

"""

import os
import sys
import argparse
from enum import Enum
import string

"""
*** SYSTEM OBJECTS
"""

class IvyObject(object):

    """ INITIALIZE IVY OBJECT """
    def __init__(self, obj_name=None, obj_type=None):
        name = obj_name if obj_name != None else self.getname()
        self.obj_name = name
        self.objdef = {
            'obj_name': name,
            'obj_type': obj_type if obj_type != None else self.getname(),
            'obj_class_name': self.getname(),
            'obj_self': self.getobj(),
        }

    """ Python Attributes """
    def getprop(self, att):
        return getattr(self, att, False)

    def setprop(self, att, val):
        setattr(self, att, val)

    def delprop(self, att):
        delattr(self, att)

    """ Ivy Object Attributes """
    def attrget(self, att):
        if att in self.objdef:
            return self.objdef[att]
        return False

    def attrset(self, att, val):
        self.objdef[att] = val

    def attrdel(self, att, val):
        self.objdef.pop(att)

    """ Ivy Object General """
    def gettype(self):
        return self.attrget('obj_type')

    def getobj(self):
        return self

    def getname(self):
        return self.__class__.__name__

    def getrepr(self):
        return self.__repr__()

    """ OPERATIONS ON OBJECTS """
    def op_mult(self, other): pass
    def op_div(self, other): pass
    def op_add(self, other): pass
    def op_sub(self, other): pass
    def op_pow(self): pass
    def op_eq(self): pass
    def op_lt(self): pass
    def op_lte(self): pass
    def op_gt(self): pass
    def op_gte(self): pass
    def op_in(self): pass

    """ OBJECT PROPERTIES """
    def istrue(self): pass
    def isnull(self): pass

    """ LIST """
    def getitem(self): pass
    def additem(self): pass
    def delitem(self): pass
    def length(self): pass

    def isfalse(self):
        return not self.istrue()

    def kill(self):
        self.__del__()

    def dynamic_package(self): pass

    def printable(self):
        return self.__repr__()

    def __repr__(self):
        return '<%s: %s, %s with 0x%08x>' % (self.getname(), self.obj_name, self.gettype(), id(self))


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
    COMP_NOT = '!='
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
    retdict.update({ tok: TokenType.BOOLEAN_CONST for tok in ['true', 'false'] })
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
            print("i'm doing it")
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
        if self.current_char != None and self.current_char in TERM_TOKENS:
            self.error('Unexpected tokenp', tok)
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
                self.error('Unexpected tokens', tok)
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

"""
*** SYSTEM OBJECTS
"""
class SystemConstants(Enum):
    SYSTEM_BUILTINS = ['length', 'type', 'repr', 'name', 'obj', 'objdef']
    BUILTIN_NAMES = ['Integer', 'Float', 'String', 'true', 'false', 'Function']
    INTEGER = 'INTEGER'
    FLOAT = 'FLOAT'
    STRING = 'STRING'
    FUNCTION = 'FUNCTION'

class Null(IvyObject):
    def __init__(self):
        super().__init__(obj_type='null')

    def isnull(self):
        return True

class DataObject(IvyObject):
    def __init__(self, obj_type, data_type=None, data=None):
        self.data = data
        self.data_type = data_type if data_type is not None else obj_type
        super().__init__(obj_type=obj_type)
        self.objdef.update({
            'obj_data_type': self.data_type,
            'obj_data': data
        })

    def isnull(self):
        return self.data == None

    def printable(self):
        return self.attrget('obj_data')

class Integer(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Integer', data=data)

class Float(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Float', data=data)

class String(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='String', data=data)

class Boolean(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='Boolean', data=data)

class TrueBoolean(Boolean):
    def __init__(self, data):
        super().__init__(data=True)

    def istrue(self):
        return True

    def printable(self):
        return 'true'

class FalseBoolean(Boolean):
    def __init__(self, data):
        super().__init__(data=False)

    def istrue(self):
        return False

    def printable(self):
        return 'false'

class Collection(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='coll', data=data)
        self.objdef.update({
            'obj_coll': []
        })

    def getitem(self, ref):
        return self.objdef['obj_coll'][ref]

    def additem(self, val):
        self.objdef['obj_coll'].append(val)

    def delitem(self, val):
        self.objdef['obj_coll'].remove(val)

class Dictionary(DataObject):
    def __init__(self, data):
        super().__init__(obj_type='dict', data=data)

class Array(DataObject):
    def __init__(self,data):
        super().__init__(obj_type='arr', data=data)

class Function(IvyObject):
    def __init__(self, params, code):
        super().__init__(obj_type='Function')
        self.params = params
        self.block = code
        self.objdef.update({
            'obj_params': params,
            'obj_code': code, # stores a code object
        })

class CodeObject(IvyObject):

    def __init__(self, code):
        super().__init__()
        self.objdef.update({
            'obj_code': code,
        })

    def invoke(self):
        pass

class Block:
    def __init__(self, block):
        self.block = block

class IvyClass(IvyObject):
    def __init__(self, obj_type='struct'):
        pass

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

# GRAMMAR
if True:
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
    pass

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
            print("do lp aren ex")
            res= self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes='Expected a closing parantheses `)` to finish expression')
        else:
            do_attr=True
            print("before call" + str(self.ctoken))
            res = self.eat_call()
            if res == False:
                self.error(token)
        while do_attr and self.ctoken.type == TokenType.DOT:
            dot = self.ctoken
            if self.match(TokenType.DOT):
                attr = self.ctoken
                if self.match(TokenType.IDENTIFIER):
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
               TokenType.COMP_EQ, TokenType.COMP_NOT, TokenType.COMP_ID, TokenType.COMP_ID_NOT]
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
        ops = [TokenType.AND]
        while self.ctoken.type in ops:
            optok = self.ctoken
            for op in ops:
                if self.match(op):
                    binfactor = BinaryOperator(binfactor, optok, self.binfactor())
        return binfactor

    def function_expression(self):
        if self.match(TokenType.FUNCTION):
            if self.match(TokenType.LPAREN):
                list_decl = None #self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error('Expected a closing parantheses')
                if not self.current(TokenType.LBRACK):
                    self.error('Expected a block to define anonymous function')
                print("get block")
                print("before block" + str(self.ctoken))
                block = self.eat_block()
                print("after block" + str(self.ctoken))
                return Function(params=list_decl, code=block)

    """ STATEMENTS """
    def statement(self):
        print("starting statement")
        token = self.ctoken
        res = self.eat_assignment()
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
        print("statement finished " + str(self.ctoken))
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
            print("doing assignment")
            if self.peek_match(TokenType.IDENTIFIER, 1):
                type, id = self.eat_declaration()
                if self.match(TokenType.EQUALS):
                    if self.ctoken.type == TokenType.FUNCTION:
                        print('do fuc as')
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
            if self.ctoken.type == TokenType.IDENTIFIER:
                params = [self.eat_type(), self.eat_id()]
                while self.match(TokenType.COMMA):
                    print("doing suc")
                    params.append(Param(self.eat_type(), self.eat_id()))
                if self.match(TokenType.COMMA): pass
        return params

    def eat_declaration(self):
        print("doing declaration")
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
            print("func block")
            if self.match(TokenType.LPAREN):
                print("open paren")
                params = self.eat_list_decl()
                print("eat list")
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
            print("look if")
            if_elif = True
            expr = self.expression()
            block = self.eat_block()
            conds.append((expr, block))
        while self.match(TokenType.ELIF):
            print("look elif")
            if not if_elif:
                self.error(token, mes='Unexpected token for conditional')
            expr = self.expression()
            block = self.eat_block()
            conds.append((expr, block))
        if self.match(TokenType.ELSE):
            print("look else")
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
        print(cond)
        return cond

    """ PROGRAM """
    def program(self, endtok=TokenType.EOF):
        prog = [self.eat_program()]
        print("first program " + str(self.ctoken))
        while self.ctoken.type != endtok:
            print("do program" + str(self.ctoken))
            prog.append(self.eat_program())
        return prog

    def eat_program(self):
        res = self.eat_conditional()
        if not res:
            res = self.statement()
        return res

    def parse(self):
        res = self.program()
        if self.ctoken.type == TokenType.EOF:
            return Program(res)
        self.error(mes='EOF Error')

"""
*** SYMBOL TABLE
*** SCOPE MANAGEMENT
"""
class Symbol:
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class ProcedureSymbol(Symbol):
    def __init__(self, name, params=None):
        super().__init__(name)
        # a list of formal parameters
        self.params = params if params is not None else []

    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__


class ScopedSymbolTable:
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INTEGER'))
        self.insert(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name),
            ('Scope level', self.scope_level),
            ('Enclosing scope',
             self.enclosing_scope.scope_name if self.enclosing_scope else None
            )
        ):
            lines.append('%-15s: %s' % (header_name, header_value))
        h2 = 'Scope (Scoped symbol table) contents'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def insert(self, symbol):
        self.log(f'Insert: {symbol.name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name, current_scope_only=False):
        self.log(f'Lookup: {name}. (Scope name: {self.scope_name})')
        # 'symbol' is either an instance of the Symbol class or None
        symbol = self._symbols.get(name)

        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)

"""
*** Main Interpreter
"""
class Interpreter:
    def __init__(self, parser):
        self.parser = parser
        self.tree = self.parser.parse()
        self.env = {}

    def visit(self, node):
        if type(node).__name__ in ['Integer','Boolean','Float','String']:
            method_name = 'visit_Data'
        else:
            method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        print("No such visitor node found: " + type(node).__name__)

    def visit_PrintStatement(self, node):
        expr = self.visit(node.expr)
        print(expr.printable() if isinstance(expr, IvyObject) else expr)
        return expr

    def visit_Program(self, node):
        for i in node.block:
            rs = self.visit(i)

    def visit_Block(self, node):
        return self.visit_Program(node)

    def visit_Function(self, node):
        return self.viist(node.block)

    def visit_Conditional(self, node):
        print("before")
        comp = self.visit(node.condition)
        if comp:
            print("visiting if block")
            return self.visit(node.ifblock)
        else:
            if node.elseblock is not None:
                print("visiting else")
                return self.visit(node.elseblock)

    def visit_CompoundStatement(self, node):
        for stmt in node.compound:
            exec_stmt = self.visit(stmt)

    def visit_BinaryOperator(self, node):
        left = self.visit(node.left).data
        right = self.visit(node.right).data
        if node.op.type == TokenType.PLUS:
            return left + right
        elif node.op.type == TokenType.MINUS:
            return self.visit(node.left) - right
        elif node.op.type == TokenType.MUL:
            return left * right
        elif node.op.type == TokenType.FLOAT_DIV:
            return left // right
        elif node.op.type == TokenType.MOD:
            return int(left) % int(right)
        elif node.op.type == TokenType.POWER:
            return left ** int(right)
        elif node.op.type == TokenType.COMP_EQ:
            return left == right
        elif node.op.type == TokenType.COMP_GT:
            return left < right
        elif node.op.type == TokenType.COMP_GTE:
            return left <= right
        elif node.op.type == TokenType.COMP_LT:
            return left > right
        elif node.op.type == TokenType.COMP_LTE:
            return left >= right
        elif node.op.type == TokenType.COMP_NOT:
            return left != right
        elif node.op.type == TokenType.AND:
            return left and right
        elif node.op.type == TokenType.OR:
            return left or right

    def visit_Data(self, node):
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
        self.env[var_name] = (var_type, None)
        print(self.env)

    def visit_VariableAssignment(self, node):
        type = node.type.type
        id = node.id.value
        value = self.visit(node.value)
        self.env[id] = (type, value)
        print(self.env)

    def visit_Assignment(self, node):
        id = node.id.value
        value = self.visit(node.value)
        self.env[id] = (self.env[id][0], value)
        print(self.env)

    def visit_VariableCall(self, node):
        var_name = node.callname
        var_value = self.env.get(var_name)[1]
        return var_value

    def visit_FunctionCall(self, node):
        func = self.env.get(node.variable.value)

    def visit_AttributeCall(self, node):
        return self.visit(node.variable).attrget(node.attribute.value)

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

class System:

    def __init__(self):
        self.exception = False

        """ Python sys module """
        self.python = sys

        """ System Trace Stack """
        self._trace = TraceStack()

        """ Main System Objects """
        self._lexer = Lexer(self._trace)

    def initsys(self, file=None, tokens=None):
        file = file if file is not None else self.createfile('<stdin>','<stdin>','')
        self._parser = Parser(self._trace, tokens, file)
        self._interp = Interpreter(self._parser)

    def tokenizefile(self, path):
        file = self.createfilefrompath(path)
        self._lexer.load(file)
        return self._lexer.tokenize()

    def createfile(self, name, path, content):
        return File(name, path, content)

    def createfilefrompath(self, path):
        fn, fp, content = self.readfile(path)
        file = File(fn, fp, content)
        # self._trace.add_trace(file, None)
        return file

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
