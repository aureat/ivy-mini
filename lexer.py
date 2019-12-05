import string
from token import Token
from tokentype import TokenType
from error import IvyLexerError

"""
*** KEYWORD GENERATOR
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

"""
*** Positional File
"""
class PositionalFile:
    def __init__(self, file):
        self.file = file
        self.contents = file.contents
        self.pos = 0
        self.line = 0
        self.col = 0

"""
*** THE LEXER
"""
class Lexer(object):
    def __init__(self, trace, file=None):
        self.trace = trace
        self.file = file
        self.current_char = None

    """ Interface Methods """

    def tokenizefile(self, file):
        self.file = PositionalFile(file)
        self.current_char = self.file.contents[0]
        return self.tokenize()

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

    """ Raising Lexer Errors """

    def error(self, mes, tok):
        file = self.file.file
        self.trace.add(file, tok)
        err = IvyLexerError(mes, self.trace)
        raise err

    """ Tokenizer Methods """

    def advance(self):
        if self.current_char == '\n':
            self.file.line += 1
            self.file.col = 0
        self.file.pos += 1
        if self.file.pos > len(self.file.contents) - 1:
            self.current_char = None
        else:
            self.current_char = self.file.contents[self.file.pos]
            self.file.col += 1

    def peek(self, more=0):
        peek_pos = self.file.pos + 1 if more==0 else self.file.pos + 2
        if peek_pos > len(self.file.contents) - 1:
            return None
        else:
            return self.file.contents[peek_pos]

    def match(self, char):
        if self.file.pos <= len(self.file.contents) - 1:
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
        token = Token(line=self.file.line, col=self.file.col)
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
        token = Token(line=self.file.line, col=self.file.col)
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
            tok = Token(line=self.file.line, col=self.file.col)
            for q in ['\'', '"']:
                if self.match(q):
                    str_tok = Token(line=self.file.line,col=self.file.col)
                    string = self.eat_string(q)
                    str_tok.type = TokenType.STRING_CONST
                    str_tok.value = string
                    tok2 = Token(line=self.file.line, col=self.file.col)
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
        return Token(type=TokenType.EOF, value=None, line=self.file.line, col=self.file.col)
