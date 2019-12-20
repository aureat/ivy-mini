from enum import Enum

"""
*** TOKEN TYPES
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
    # MANAGEMENT STATEMENTS
    PACKAGE = 'package'
    IMPORT = 'import'
    USE = 'use'
    GLOBAL = 'global'
    # OBJECT CONSTRUCTION
    FUNCTION = 'function'
    STRUCT = 'struct'
    # DATA TYPE Literals
    TRUE = 'true'
    FALSE = 'false'
    NULL = 'null'
    # CONDITIONALS
    IF = 'if'
    ELSE = 'else'
    RETURN = 'return'
    AND = 'and'
    OR = 'or'
    NOT = 'not'
    # STATEMENTS
    REMOVE = 'remove'
    # LOOPING
    WHILE = 'while'
    FOR = 'for'
    BREAK = 'break'
    CONTINUE = 'continue'
    # TYPE LITERALS
    IDENTIFIER = 'IDENTIFIER'
    INTEGER_CONST = 'INTEGER_CONST'
    FLOAT_CONST = 'FLOAT_CONST'
    STRING_CONST = 'STRING_CONST'
    BOOLEAN_CONST = 'BOOLEAN_CONST'
    # EOF
    EOF = 'EOF'
