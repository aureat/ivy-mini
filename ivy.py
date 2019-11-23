"""
***  IVY Language Interpreter
***  Main Class

TODO:
(+) Implement a tokenizer that stores the index of the first character and line number in program
(+) Line and character (wise) tokenizer
( ) Implement syntax error checking / handling inside the interpreter
(+) Error handler function with pretty printing of line number etc.
( ) Integrate the trace collector and implement a stack trace error handler
( ) Locals and Globals
( ) Variable declaration without explicit type declaration
( ) Implement and/or/xor
( ) String operations / Concatenations
( ) Array indexing using the square bracket notation (inside the factor)
( ) Array element assignment (figure out the best data structure to implement arrays)
( ) Add the dot attribute functionality

( ) Data Types as Objects
( ) Expressions as array elements
( ) Open close paranthesis counter
( ) Add support for multiple-line line-pass programs
( ) File Object to store reference

Environment has local and global references, internal system package implementation

Advanced features
( ) Add attribute-functions to objects
( ) Dynamic Package Creation

Syntactic Elements and Advancements
( ) Nested blocks implementation
( ) Expressions of type ---> 1 if x > 0 else 0

"""

import os
import sys
import string
import argparsew
from enum import Enum

"""
Language Model
func [name] = function( [params] ) { [code] };

Object Model
Object (IvyObject) => ClassObject | ...
  - func (Function)
  - CodeObject (CodeObject)
  - str (String)
  - int (Integer)
  - float (Float)
  - null (Null)
  - coll (Collection)
  - dict (Dictionary)
  - arr (Array)
  - Error (Error)
"""

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
        })

    def get_trace(self):
        trace = []
        for i in self.objdef['trace']:
            trace.append({
                'filename': i['file'].name,
                'content': i['file'].content,
                'line_content': i['file'].get_line(line),
                'line': i['token'].line,
                'col': i['token'].col,
                'name': '<module>'
            })
        return trace

"""
*** ERROR Objects
"""
class ErrorCode(Enum):
    UNEXPECTED_TOKEN = 'Unexpected token'
    ID_NOT_FOUND = 'Identifier not found'
    STRING_NOT_CLOSED = 'String not completed'

class Error(IvyObject):
    def __init__(self, desc, name, trace = TraceStack(), token=None, type=None):
        super().__init__()
        self.objdef.update({
            'error_name': name,
            'error_details': desc,
            'error_token': token,
            'error_trace': trace,
            'error_type': type})

    def get_error(self):
      error = '\nTraceback:\n'
      for i in self.get_attr('error_trace').get_trace():
          line = i['line_content']
          char = i['col']
          trace_beg = min(30,len(line[:char]))
          trace_end = min(30,len(line[char:]))
          error += '   File {}, line {}, col {} in {}\n'.format(i['filename'], i['line']+1, i['col']+1, i['name'])
          error += '\t' + line[char-trace_beg:char+trace_end] + '\n'
          if i['col']:
              error += '\t' + " " * i['col'] + '^\n'
      error += '{}: {}\n'.format(self.objdef['error_name'], self.objdef['error_details'])
      return error

class SyntaxError(Error):
    def __init__(self, desc, trace=TraceStack()):
        super().__init__(desc, 'SyntaxError', trace)

class ParseError(Error):
    def __init__(self, desc, trace=TraceStack()):
        super().__init__(desc, 'ParseError', trace)

class LexerError(Error):
    def __init__(self, desc, trace=TraceStack()):
        super().__init__(desc, 'LexerError', trace)

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
    LBRACK = '['
    RBRACK = ']'
    L_SQ_BRACK = '{'
    R_SQ_BRACK = '}'
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
    EOF = 'EOF'

class Token:
    def __init__(self, type=None, value=None, line=0, col=0):
        self.type = type
        self.value = value
        self.line = line
        self.col = col
        self.length = len(value) if value is not None else None

    def __str__(self):
        return 'Token({type}, {value}, pos={line}:{col})'.format(type=self.type, value=str(self.value), line=self.line, col=self.col)

    def __repr__(self):
        return self.__str__()

def language_keywords():
    token_types = list(TokenType)
    ind_start = token_types.index(TokenType.PACKAGE)
    ind_end = token_types.index(TokenType.CONTINUE)
    return {token_type.value: token_type for token_type in token_types[ind_start:ind_end+1]}

"""
*** LEXER CONSTANTS
"""
ASCII_LETTERS = string.ascii_letters
DIGITS = string.digits
ALPHA_NUMERIC = ASCII_LETTERS + DIGITS
RESERVED_KEYWORDS = language_keywords()
SINGLE_TOKENS = ['+', '/', '%', '(', ')', ';', ':', ',', '[', ']', '{', '}']

"""
*** FILES AND PACKAGES
"""

class Package(IvyObject): pass

class DynamicPackage(Package): pass

class File(IvyObject):
    def __init__(self, name, path, content, package=None):
        self.name = name
        self.path = path
        self.content = content
        self.package = package

    def split_lines(self):
        return self.content.split('\n')

    def get_line(self, ind):
        return self.split_lines()[ind]

"""
*** LEXER AND TOKENIZER
"""
class Lexer(object):

    def __init__(self, file):
        self.file = file
        self.program = file.content
        self.pos = 0
        self.current_char = self.program[self.pos]
        self.line = 0
        self.col = 0
        self.current_token = None

    def error(self, mes, tok):
        trace = TraceStack()
        trace.add_trace(self.file, tok)
        err = LexerError(mes, trace)
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
        if self.current_char == '.':
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

    def eat_id(self):
        token = Token(line=self.line, col=self.col)
        value = ''
        if self.current_char.isalpha():
            value += self.current_char
            self.advance()
        while self.current_char != None and self.current_char.isalnum():
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
        if self.current_char != None and not self.current_char.isalnum() and not self.current_char.isspace():
            self.error('Unexpected token', tok)
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
            if self.current_char.isalpha():
                return self.eat_id()
            elif self.current_char.isdigit():
                return self.number()
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
                        return self.rtoken(tok, '.')
                    return self.rtoken(tok, '..')
                return self.rtoken(tok, '.')
            for i in SINGLE_TOKENS:
                if self.match(i):
                    return self.rtoken(tok, i)
            if self.current_char != None:
                self.error('Unexpected token', tok)
        return Token(type=TokenType.EOF, value=None)

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
*** System Classes
"""
class SystemConstants(Enum):
    SYSTEM_BUILTINS = ['length', 'type', 'repr', 'name', 'obj', 'objdef']
    BUILTIN_NAMES = ['Integer', 'Float', 'String', 'true', 'false', 'Function']
    INTEGER = 'INTEGER'
    FLOAT = 'FLOAT'
    STRING = 'STRING'
    FUNCTION = 'FUNCTION'

class ARType(Enum):
    PROGRAM   = 'PROGRAM'

class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def peek(self):
        return self._records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()


class ActivationRecord:
    def __init__(self, name, type, nesting_level):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key)

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
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

"""
*** SYSTEM OBJECTS
"""
class IvyObject(object):

    """ INITIALIZE IVY OBJECT """
    def __init__(self, obj_name=None, obj_type=None):
        name = obj_name if obj_name != None else self.get_name()
        self.obj_name = name
        self.objdef = {
            'obj_name': name,
            'obj_type': obj_type if obj_type != None else self.get_name(),
            'obj_class_name': self.get_name(),
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
        return self.get_attr('obj_type')

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

    def __repr__(self):
        return '<{}: {}, {} at {}>'.format(self.get_name(), self.obj_name, self.get_type(), id(self))

class Null(IvyObject):
    def __init__(self):
        super().__init__(obj_type='null')

    def isnull(self):
        return True

class DataObject(IvyObject):
    def __init__(self, obj_type, data_type=None, data=None):
        data_type = data_type if data_type is not None else obj_type
        super().__init__(obj_type=obj_type)
        data_type
        self.objdef.update({
            'obj_data_type': data_type,
            'obj_data': data
        })

class Integer(DataObject):
    def __init__(self, data=None):
        super().__init__(obj_type='int', data=data)

class Float(DataObject):
    def __init__(self):
        super().__init__(obj_type='float', data=data)

class String(DataObject):
    def __init__(self):
        super().__init__(obj_type='str', data=data)

class Boolean(DataObject):
    def __init__(self):
        super().__init__(obj_type='bool', data=data)

class Collection(DataObject):
    def __init__(self):
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
    def __init__(self):
        super().__init__(obj_type='dict', data=data)

class Array(DataObject):
    def __init__(self):
        super().__init__(obj_type='arr', data=data)

class Function(IvyObject):
    def __init__(self, name, params, code, ):
        super().__init__(obj_name=name,obj_type='func')
        self.objdef.update({
            'obj_params': params,
            'obj_code': code, # stores a code object
        })

    def obj_call(self):
        pass

    def invoke(self):
        self.obj_call()

class CodeObject(IvyObject):

    def __init__(self, code):
        super().__init__()
        self.objdef.update({
            'obj_code': code,
        })

    def invoke(self):
        pass

class IvyClass(IvyObject):
    def __init__(self, obj_type='struct'):
        pass

"""
*** PARSER
*** LANGUAGE MODELING

Parser Nodes
- Data
- VariableAssign
- VariableAccess
- AttributeAccess
- CodeBlock
- FunctionBlock
- FunctionDeclaration
- Program
- BinaryOperator
- UnaryOperator
"""

class Parser(IvyObject):

    def __init__(self, file, tokens):
        file = file
        self.lexer = Lexer()
        self.tokens = self.lexer.tokenize()
        self.idtoken = 0
        self.ctoken = self.tokens[0]

    def get_token(self):
        self.idtoken += 1
        self.ctoken = self.tokens[self.idtoken]
        if self.ctoken.type = TokenType.EOF:
            return False

""" Data Objects """
class Data:
    def __init__(self, tok):
        self.value = tok

class VariableType:
    def __init__(self, tok):
        self.type = tok

class VariableAssign:
    def __init__(self, type, tok, value):
        self.type = type
        self.variable = tok
        self.value = value

class VariableAccess:
    def __init__(self, name):
        self.variable = name

class AttributeAccess:
    def __init__(self, var, att):
        self.variable = var
        self.attribute = att

""" Functional Objects """
class CodeBlock:
    def __init__(self, code=None):
        self.code = code if code != None else []

class FunctionBlock:
    def __init__(self, declarations, block):
        self.declarations = declarations
        self.code = block

class Program:
    def __init__(self, name, block):
        self.name = name
        self.block = block

class Parameter:
    def __init__(self, type, name):
        self.type = type
        self.param = name

class Parameters:
    def __init__(self, params):
        self.params = params if params is not None else []

class FunctionDeclaration:
    def __init__(self, func, params, code):
        self.func = func
        self.params = params
        self.code = code

class FunctionCall:
    def __init__(self, func, params, token):
        self.name = func
        self.params = params
        self.token = token

class Return:
    def __init__(self, toret):
        self.return = toret

""" Operators """
class BinaryOperator:
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class UnaryOperator:
    def __init__(self, op, node):
        self.op = op
        self.node = node

""" Control Flow """
class Conditional:
    def __init__(self, cond, ifb, elseb):
        self.condition = cond
        self.ifblock = ifb
        self.elseblock = elseb

class WhileLoop:
    def __init__(self, cond, body, retnull):
        self.condition = cond
        self.body = body
        self.retnull = retnull

class ForLoop:
    def __init__(self, var, from, to, step, body, retnull):
        self.variable = var
        self.from = from
        self.to = to
        self.step = step
        self.body = body
        self.retnull = retnull

class ContinueLoop: pass
class BreakLoop: pass


"""
*** Main Interpreter
"""

class Interpreter():

    def __init__(self, tree):
        self.tree = tree
        self.call_stack = CallStack()

    def log(self, msg):
        if _SHOULD_LOG_STACK:
            print(msg)

    def binary_op(self, node):
        if node.op.type == TokenType.PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == TokenType.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == TokenType.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == TokenType.INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))

    def program(self, node):
        program_name = node.name
        self.log(f'ENTER: PROGRAM {program_name}')

        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {program_name}')
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def assign(self, node):
        var_name = node.left.value
        var_value = self.visit(node.right)

        ar = self.call_stack.peek()
        ar[var_name] = var_value

"""
*** Ivy System
"""

class System(IvyObject):

    def __init__(self):
        super().__init__(self)
        self.objdef.update({
            'stdin': sys.stdin,
            'stdout': sys.stdout,
            'stderr': sys.stderr,
        })

    def push_error(self): pass

    def dynamic_package(self): pass

class IvyConsole:
    def __init__(self):
        pass

if __name__ == '__main__':
    program = 'if this and that -> {\n "Altun  }'
    fl = File('<stdin>', '.', program)
    pars = Parser(fl)
    for i in pars.tokens:
        print(i)
