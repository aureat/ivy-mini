"""
***  IVY Language Interpreter
***  Main Class

TO DO:
( ) Start the parser and the Interpreter
( ) Where do lexer objects belong to? Parser vs Interpreter

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
( ) Implement the system package
( ) Determine the functional structure of this ivy implementation
( ) TraceStack vs CallStack
( ) TraceStack object inheritance by system objects

"""

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
        self.toreturn = toret


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
    def __init__(self, var, fromv, tov, step, body, retnull):
        self.variable = var
        self.fromv = fromv
        self.tov = tov
        self.step = step
        self.body = body
        self.retnull = retnull

class ContinueLoop: pass
class BreakLoop: pass

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

""" SYSTEM BUILTIN Literal Classes (Immutable)

Integer, Float, String, Function,

not to be confused with type tokens: int, float, str, func, coll

* boolean literal: true | false
* string literal: " ... " | ' ... '
* number literal: [0-9]*(.[0-9]+)?

"""

"""
LANGUAGE GRAMMAR.

package = package [identifier]
import = import [identifier]

program := list-statements | function-declaration | conditional | while-loop | for-loop | program

statement := (declaration | assignment | expression | package | import) ";"
list-statements := (statement)*

assignment := [declaration] = [expression]
declaration := [type] [identifier]
function-declaration := func [identifier]: "(" [list-parameters] ")" ( -> "(" [list-parameters] ")" )? [block]

expression := [unary] (< | <= | > | >= | == | ===) [unary]
unary := [term] ((+|-) [term])*
term := [factor] ((*|/|%) [factor])*
factor := [integer] | [string] | [function-call] | [attribute-call] | [index-call] | "(" [expression] ")"

list-expression := ([expression],)*
collection := [ list-expression ]

type := [identifier] | int | float | str | bool | coll | arr | dict | func
list-parameters := ([declaration],)*
function-block := function ( [list-parameters] ) [block]
block := { [program] }

variable-call := [identifier]
function-call := ([identifier] | [function-block]) "(" [list-expression] ")"
attribute-call := [identifier] "." [identifier]
index-call := ([identifier] | [collection]) "[" [expression] "]"

range := [integer] .. [integer]
iteration := [identifier] in ([range] | [collection])

conditional := if [expression] [block] (elif [expression] [block])* else [block]
while-loop := while [expression] [block]
for-loop := for [iteration] [block]

identifier := [a-zA-Z_]([a-zA-Z0-9_])*
integer := [0-9]+ (TokenType.INTEGER_CONSTANT)
float := [0-9]*(.[0-9]+)? (TokenType.FLOAT_CONSTANT)
boolean := true | false
string := " [.*] " | ' [.*] '

"""
