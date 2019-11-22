"""
***  IVY Language Interpreter
***  Main Class

TODO:
( ) Implement a tokenizer that stores the index of the first character and line number in program
( ) Line and character (wise) tokenizer
( ) Implement syntax error checking / handling inside the interpreter
( ) Error handler function with pretty printing of line number etc.
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

class IvyObject(object):

    def __init__(self, obj_name=None, obj_type=None):
        name = obj_name if obj_name != None else self.get_name()
        self.obj_name = name
        self.objdef = {
            'obj_name': name,
            'obj_type': obj_type if obj_type != None else self.get_name(),
            'obj_class_name': self.get_name(),
        }

    def get_prop(self, att):
        try:
            return getattr(self, att)
        except AttributeError:
            return False

    def get_attr(self, att):
        if att in self.objdef:
            return self.objdef[att]
        return False

    def get_type(self):
        return self.get_attr('obj_type')

    def get_obj(self):
        return self

    def get_name(self):
        return self.__class__.__name__

    def dynamic_package(self): pass

    def __repr__(self):
        return '<{}: {}, {} at {}>'.format(self.get_name(), self.obj_name, self.get_type(), id(self))

class DataObject(IvyObject): pass

class Null(IvyObject):
    def __init__(self):
        super().__init__(obj_type='null')

class Integer(IvyObject):
    def __init__(self):
        super().__init__(obj_type='int')

class Float(IvyObject):
    def __init__(self):
        super().__init__(obj_type='float')

class String(IvyObject):
    def __init__(self):
        super().__init__(obj_type='str')

class Boolean(IvyObject):
    def __init__(self):
        super().__init__(obj_type='bool')

class Collection(IvyObject):
    def __init__(self):
        super().__init__(obj_type='coll')

class Dictionary(IvyObject):
    def __init__(self):
        super().__init__(obj_type='dict')

class Array(IvyObject):
    def __init__(self):
        super().__init__(obj_type='arr')

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
          line = i['content'].split('\n')[i['line']]
          char = i['col']
          trace_beg = min(30,len(line[:char]))
          trace_end = min(30,len(line[char:]))
          error += '   File {}, line {} by {}\n'.format(i['filename'], i['line']+1, i['name'])
          error += '\t' + line[char-trace_beg:char+trace_end] + '\n'
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
*** Token
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
    TRUE = 'true'
    FALSE = 'false'
    # CONTROL FLOW
    IF = 'if'
    ELSE = 'else'
    ELIF = 'elif'
    RETURN = 'return'
    AND = 'and'
    OR = 'or'
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
*** CONSTANT DATA
"""

ASCII_LETTERS = string.ascii_letters
DIGITS = string.digits
ALPHA_NUMERIC = ASCII_LETTERS + DIGITS
RESERVED_KEYWORDS = language_keywords()
SINGLE_TOKENS = ['"', '\'', '+', '/', '%', '(', ')', ';', ':', ',', '[', ']', '{', '}']

"""
*** File
"""

class Package(object): pass

class DynamicPackage(Package): pass

class File(object):
    def __init__(self, name, path, content, package=None):
        self.name = name
        self.path = path
        self.content = content
        self.package = package

"""
*** Lexer
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
        self.advance()
        value = ''
        while self.current_char != None and self.current_char != quote:
            value += self.current_char
            self.advance()
        return value

    def rtoken(self, tok, val):
        if not self.current_char.isspace():
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

"""
*** Main Interpreter
"""

class Interpreter(IvyObject): pass

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

if __name__ == '__main__':
    program = 'if this and that -> {\n "Altun \n}'
    fl = File('<stdin>', '.', program)
    lex = Lexer(fl)
    for i in range(10):
        print(lex.get_token())
