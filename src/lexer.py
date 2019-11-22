import re

class TokenType(object):

    def __init__(self, name, val=None, lst=[]):
        self.name = name
        self.val = val
        lst.append(self)

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return str(self.name)

class Token(object):

    def __init__(self, type, value, start_pos, line, length):
        self.type = type
        self.value = value
        self.start_pos = start_pos
        self.end_pos = start_pos + len(value)
        self.line = line
        self.length = length

    def __str__(self):
        return 'Token(' + str(self.type) + ', `' + str(self.value) + '`, ' + str(self.start_pos) + ', ' + str(self.line) + ', ' + str(self.length) + ')'

    def __repr__(self):
        return self.__str__()

tokens = []
all_tokens = [
    ('BOOL', 'bool'), ('INTEGER', 'int'), ('FLOAT', 'float'), ('STRING', 'str'), ('COLLECTION', 'col'), ('ARRAY', 'arr'), ('FUNCTION', 'func'), ('CLASS', 'class'),
    ('TRUE', 'true'), ('FALSE', 'false'),
    ('PLUS', '+'), ('MINUS', '-'), ('MULT', '*'), ('DIV', '/'), ('MOD', '%'), ('POW', '**'),
    ('IF', 'if'), ('ELSE', 'else'), ('ELIF', 'elif'),
    ('EQUALS', '='), ('COMP_EQUALS', '=='),  ('D_QUOTE', '"'),
    ('OPEN_PAREN', '('), ('CLOSE_PAREN', ')'), ('OPEN_BRACK', '{'), ('CLOSE_BRACK', '}'),
    ('OPEN_SQ_BRACK', '['), ('CL0SE_SQ_BRACK', ']'), ('COMMA', ','), ('SEMICOLON', ';'), ('DOT', '.'),
    ('WHILE', 'while'), ('FOR', 'for'), ('RETURN', 'return'), ('BREAK', 'break'), ('CONTINUE', 'continue'),
    ('COMP_LT', '>'), ('COMP_GT', '<'), ('COMP_LTE', '>='), ('COMP_GTE', '<='),
    ('INCLUDE', 'include'),
]

EOF = TokenType('EOF', tokens)
IDENTIFIER = TokenType('IDENTIFIER')
NUMBER = TokenType('NUMBER')
STRING = TokenType('STRING')
S_QUOTE = TokenType('S_QUOTE', '\'', tokens)

class Lexer(object):

    def __init__(self, trace_col=None, input=None):
        self.program = input
        self.tokens = []
        self.trace_col = trace_col

    def tokenize_program(self, program):
        seperated = program.split('\n')
        tagged_lines = enumerate(seperated)
        full_line = program
        return self.tokenize_index(program)

    def tokenize_index(self, prog):
        tokens = self.tokenize_line(prog)
        index = prog.index
        offsets = []
        append = offsets.append
        running_offset = 0
        line_count = 0
        for token in tokens:
            word_offset = index(token, running_offset)
            word_len = len(token)
            running_offset = word_offset + word_len
            append((token, word_offset, 0,running_offset))
        return offsets

    def tokenize_line(self, program):
        self.program = program
        tokstr = filter(None, re.split(r'\s* ( (\/\/.*\n*) | (\'.*\') | (\".*\") | [a-zA-Z_][a-zA-Z0-9_]* | [0-9]*\.?[0-9]+ |(==)|(>=)|(<=)|(\*\*)| [(){}\[\];\+\-=/%\.,<:>] ) \s*', program))
        # self.tokens = []
        # [self.tokens.append(i) for i in tokstr if i not in self.tokens]
        self.tokens = ' '.join(tokstr).split()
        print(self.tokens)
        return self.tokens

    def tokenize(self, tokens_in):
        token_str = {}
        [token_str.update({i.val: i.name}) for i in tokens]
        token_list = [Token(token_str[i[0]], i[0], i[1], i[2], i[3]) if i[0] in token_str.keys() else Token(None, i[0], i[1], i[2], i[3]) for i in tokens_in ]
        return token_list

    def lex(self, program):
        return self.tokenize(self.tokenize_program(program))

    def show(self, program):
        for i in self.lex(program):
            print(i)

def add_tokens():
    for tok in all_tokens:
        exec('{name}=TokenType(\'{name}\',\'{val}\',tokens)'.format(name=str(tok[0]),val=str(tok[1])))

if __name__ == '__main__':
    add_tokens()
    lexer = Lexer()
    lexer.show(" if x > 3 { \n print (3 + ' hey'); \n } func getdef = (int a, int b) \n { return a + b; }")
