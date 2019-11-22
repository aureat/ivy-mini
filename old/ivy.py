"""
*** IVY Language [V1.0]
***
*** COMPONENTS:
***    (1) Izy main
***    (2) Lexer
***    (3) System (Interpreter)
***    (4) Trace Collector
***    (5) Token class
***    (6) TokenType Class
***

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

import os, sys, re, string

#
# OBJECTS START
#

class IvyObject(object): pass

class Function(IvyObject): pass

class IvyClass(IvyObject): pass

class Package(object): pass

class File(object): pass

class Inspect(object): pass

class SystemPackage(Package): pass

class IOPackage(Package): pass

class Interpreter(object): pass

class Environment(object): pass


#
# OBJECTS END
#

class TokenType(object):

    def __init__(self, name, val=None, lst=[]):
        self.name = name
        self.val = val
        lst.append(self)
        lst.sort(key=lambda kind: -len(kind.val) if kind.val else 0)

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return self.__str__()

class Token(object):

    def __init__(self, type, value, start_pos = None, line= None):
        self.type = type
        self.value = value
        # self.start_pos = start_pos
        # self.end_pos = start_pos + len(value)
        # self.line = line

    def __str__(self):
        return 'Token(' + str(self.type) + ', ' + str(self.value) + ')'

    def __repr__(self):
        return self.__str__()

tokens = []
all_tokens = [('COMMENT_BEGIN', '//'),
    ('BOOL', 'bool'), ('INTEGER', 'int'), ('FLOAT', 'float'), ('STRING', 'str'), ('COLLECTION', 'col'), ('ARRAY', 'arr'), ('FUNCTION', 'func'), ('CLASS', 'class'),
    ('TRUE', 'true'), ('FALSE', 'false'),
    ('PLUS', '+'), ('MINUS', '-'), ('MULT', '*'), ('DIV', '/'), ('MOD', '%'), ('POW', '**'),
    ('IF', 'if'), ('ELSE', 'else'), ('ELIF', 'elif'),
    ('EQUALS', '='), ('COMP_EQUALS', '=='),  ('D_QUOTE', '"'),
    ('OPEN_PAREN', '('), ('CLOSE_PAREN', ')'), ('OPEN_BRACK', '{'), ('CLOSE_BRACK', '}'),
    ('OPEN_SQ_BRACK', '['), ('CLOSE_SQ_BRACK', ']'), ('COMMA', ','), ('SEMICOLON', ';'), ('DOT', '.'),
    ('WHILE', 'while'), ('FOR', 'for'), ('RETURN', 'return'), ('BREAK', 'break'), ('CONTINUE', 'continue'),
    ('COMP_LT', '>'), ('COMP_GT', '<'), ('COMP_LTE', '>='), ('COMP_GTE', '<='),
    ('COMP_AND', 'and'), ('COMP_OR', 'or'), ('COMP_IN', 'in'),
    ('INCLUDE', 'include'), ('THREE_DOTS', '...')
]

EOF = TokenType('EOF', tokens)
IDENTIFIER = TokenType('IDENTIFIER')
NUMBER = TokenType('NUMBER')
STRINGT = TokenType('STRINGT')
S_QUOTE = TokenType('S_QUOTE', '\'', tokens)

class TraceCollector(object):

    def __init__(self, lang_cl):
        self.iter = 0
        self.trace = {}
        self.lang_cl = lang_cl

    def push_trace(self, trace, new_iter=0):
        if new_iter:
            self.iter += 1
            trace[self.iter] = []
        self.trace[self.iter].append(trace)

    def iterate(self):
        self.iter += 1

    def push_error(self, name, message, token=None, char=None, line=None):
        if token:
            line = token.line
            char = token.start_pos
        program = self.lang_cl.program
        filename = self.lang_cl.filename
        filepath = self.lang_cl.filepath
        print(src.upper() + ' Error: ' + message + ' [at line ' + str(line) + ']')
        trace_beg = min(25,len(line[:char]))
        trace_end = min(25,len(line[char:]))
        print("Trace: \"" + ''.join(self.env['program'][opref-trace_beg:opref+trace_end]) + "\"")
        for i in range(trace_beg + 7):
            print(" ", end="")
        print("^")

class Lexer(object):

    def __init__(self, trace_col, input=None):
        self.program = input
        self.tokens = []
        self.trace_col = trace_col

    def split_lines(self): pass

    def tokenize_line(self, program):
        self.program = program
        tokstr = filter(None, re.split(r'\s*(\/\/.*\n*)|((\'.*\')|(\".*\")|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]*\.?[0-9]+|(\.\.\.)|(==)|(>=)|(<=)|(\*\*)|[(){}\[\];\+\-=/%\.,<:>])\s*', program))
        print("regex")
        print(list(tokstr))
        self.tokens = []
        [self.tokens.append(i) for i in tokstr if i not in self.tokens]
        self.tokens = ' '.join(self.tokens).split()
        print(self.tokens)
        return self.tokens

    def tokenize(self, tokens_in):
        token_str = {}
        [token_str.update({i.val: i.name}) for i in tokens]
        print(token_str)
        token_list = [Token(token_str.get(i[0]), i[0]) for i in tokens_in]
        print(token_list)
        return token_list

    def lex(self, program):
        return self.tokenize(self.tokenize_line(program))

class SystemInstance(object): pass

class System(object):

    def __init__(self):
        self.trace_col = TraceCollector(self)
        self.program = None
        self.lexer = Lexer(self.trace_col)
        self.token_types = {
            'declaration': [FUNCTION, INTEGER, FLOAT, STRING, ARRAY, BOOL],
            'function': [FUNCTION],
            'condition': [IF, ELSE, ELIF],
            'loop': [FOR, WHILE],
            'loop_break': [BREAK],
            'loop_continue': [CONTINUE],
            'open_block': [OPEN_BRACK],
            'close_block': [CLOSE_BRACK],
            'open_sq': [OPEN_SQ_BRACK],
            'close_sq': [CLOSE_SQ_BRACK],
            'string_wrap': [D_QUOTE, S_QUOTE],
            'assignment': [EQUALS],
            'comparison': [COMP_EQUALS, COMP_LT, COMP_GT, COMP_LTE, COMP_GTE],
            'term_op': [MINUS, PLUS],
            'factor_op': [MULT, DIV, MOD, POW],
            'add_inv': [MINUS],
            'open_paren': [OPEN_PAREN],
            'close_paren': [CLOSE_PAREN],
            'return_op': [RETURN],
            'attr_op': [DOT],
            'list_sep': [COMMA],
            'bool': [TRUE, FALSE],
            'file_include': [INCLUDE]
        }
        self.env = {}
        self.args = []
        self.tokens = None

    def push_error(): pass

    def run_code(self, program, args = None):
        self.program = program
        if args:
            self.args = args
        self.tokens = self.lexer.lex(program)
        if len(self.tokens) > 0:
            return self.do_program()
        return None

    def replace_var(self, lst, replace, rwith):
        return map(lambda x: rwith if x.value == str(replace) else x, lst)

    def store_var(self, dict):
        dict.update({'builtin': False})
        self.env[dict['name']] = dict

    def get_var(self, name):
        return self.env[name]

    def reset_var_params(self, name):
        self.env[name]['params'] = []

    def push_error(self): pass

    def get_token(self):
        if len(self.tokens) > 0:
            return tokens[0]
        return EOF

    def pop_token(self):
        if len(self.tokens) > 0:
            first = self.get_token()
            self.tokens = self.tokens[1:]
            return first
        return EOF

    def convert_number(self, num):
        try:
            num = int(num)
        except ValueError:
            try:
                num = float(num)
            except ValueError:
                print('convert_number error')
        return num

    def convert_int(self, num):
        try:
            num = int(num)
        except ValueError:
            print('convert_int error')
        return num

    def is_letter(self):
        return self.get_token().value[0] in string.ascii_letters

    def is_digit(self):
        return self.get_token().value[0] in string.digits

    def is_token(self, type):
        print(self.get_token())
        return self.get_token().type in self.token_types[type]

    def remove_comment(self):
        if(self.get_token().startswith('//')):
            self.pop_token()

    def eat_number(self):
        if(self.is_digit()):
            return self.convert_number(self.pop_token().value)
        return False

    def eat_identifier(self):
        if(self.is_letter()):
            return self.pop_token().value
        return False

    def eat_string(self):
        if self.get_token().value.startswith('\'') or self.get_token().value.startswith('"'):
            return self.pop_token().value[1:-1]
        return False

    def eat_array(self):
        if self.is_token('open_sq'):
            arr_tok = ''
            while self.get_token().type == CLOSE_SQ_BRACK:
                arr_tok += str(self.pop_token().value)
        return False

    def eat_until(self, char):
        ret = ""
        while self.get_token().value != char:
            ret += self.pop_token()
        return ret

    def eat_block(self):
        ret = []
        self.pop_token()
        while self.get_token().type != CLOSE_BRACK:
            ret.append(self.pop_token())
        self.pop_token()
        return ret

    def eat_run_return(self):
        self.pop_token()
        ret = []
        while not self.is_token('close_block') and len(self.tokens) > 0:
            ret.append(this.pop_token())
        interpreter = Interpreter(self.env)
        return interpreter.run_code(' '.join(ret))

    def factor(self):
        final_factor = self.eat_number()
        if final_factor != False:
            return final_factor
        final_factor = self.eat_string()
        if final_factor != False:
            return final_factor
        if self.is_token('add_inv'):
            self.pop_token()
            return -self.factor()
        if self.is_token('bool'):
            return self.pop_token().value
        if self.is_token('open_paren'):
            self.pop_token()
            factor = self.expression()
            self.pop_token()
            return factor
        if self.is_token('open_sq'):
            self.pop_token()
            arr_factor = self.expression()
            self.pop_token()
            return arr_factor
        final_factor = self.eat_identifier()
        if final_factor != False and str(inal_factor) in self.env.keys():
                var = self.get_var(final_factor)
                if var['type'] == 'FUNC':
                    return self.functionCall(var)
                else:
                    return var['body']
        print("Error: no such factor")

    def term(self):
        final_term = self.factor()
        while self.is_token('factor_op'):
            if self.pop_token().type == MULT:
                final_term *= this.factor()
            elif self.pop_token().type == DIV:
                final_term /= self.factor()
            elif self.pop_token().type == MOD:
                final_term %= self.factor()
        return final_term

    def expression(self):
        final_exp = self.term()
        while self.is_token('term_op'):
            if self.pop_token().type == PLUS:
                final_exp += self.term()
            elif self.pop_token().type == MINUS:
                final_exp += self.term()
        return final_exp

    def comparison(self):
        first = self.expression()
        if self.is_token('comparison'):
            if self.pop_token().type == COMP_EQUALS:
                return first == self.expression()
            elif self.pop_token().type == COMP_GT:
                return first < self.expression()
            elif self.pop_token().type == COMP_LT:
                return first > self.expression()
            elif self.pop_token().type == COMP_GTE:
                return first <= self.expression()
            elif self.pop_token().type == COMP_LTE:
                return first >= self.expression()
        return first

    def read_params(self):
        paramtext = self.eat_until(')')
        params = [tuple(i.split()) for i in paramtext.split(',')]
        return params

    # def function_call(self, func):
    #     self.pop_token()
    #     self.read_params()


# Interpreter.prototype.functionCall = function(currentFunction) {
# 	if (!this.isOpeningParen()) throw new Error("A function call must have arguments wrapped in parentheses!");
# 	this.get();
# 	let currentArguments = this.consumeUntil(")", "array");
# 	if (currentArguments.length) currentArguments = currentArguments.join("").split(",");
# 	if (!this.isClosingParen()) throw new Error("A function call must have arguments wrapped in parentheses!");
# 	this.get();
# 	if (currentFunction.isNative) {
# 		currentFunction.params.push(this)
# 		for (let i in currentArguments) {
# 			let funcName = currentArguments[i].substring(0,currentArguments[i].indexOf('('));
# 			if (this.getVariable(currentArguments[i])) {
# 				currentFunction.params.push(this.getVariable(currentArguments[i]));
# 			}
# 			else if (this.getVariable(funcName)) {
# 				let newInterpreter = new Interpreter(this.memory);
# 				currentFunction.params.push(newInterpreter.input(currentArguments[i]));
# 			}
# 			else {
# 				currentFunction.params.push(currentArguments[i]);
# 			}
# 		}
# 		let returnVal = currentFunction.body(currentFunction.params);
# 		this.resetParams(currentFunction.name);
#
# 		return returnVal;
# 	}
# 	let otherInterpreter = new Interpreter(this.memory);
# 	let bodyToParse = currentFunction.body.slice(0);
# 	for (let j = 0; j < currentArguments.length; j++) {
# 		let currentArgument;
# 		if (this.getVariable(currentArguments[j]) && this.getVariable(currentArguments[j]).type === "fn") {
# 			currentArgument = currentArguments[j];
# 		} else {
# 			currentArgument = otherInterpreter.input(currentArguments[j]) || currentArguments[j];
# 		}
# 		let parameterToReplace = currentFunction.params[j].name;
# 		switch(currentFunction.params[j].type) {
# 			case "num":
# 				if (typeof parseFloat(currentArgument) !== "number" || isNaN(parseFloat(currentArgument))) throw new Error("Functions should only be called with parameters of the correct type!");
# 				break;
# 			case "str":
# 				if (currentArgument[0] !== '"') throw new Error("Functions should only be called with parameters of the correct type!");
# 				break;
# 			case "bool":
# 				if (currentArgument !== "true" && currentArgument !== "false") throw new Error("Functions should only be called with parameters of the correct type!");
# 				break;
# 			case "fn":
# 				if (!this.getVariable(currentArgument) || this.getVariable(currentArgument).type !== "fn") throw new Error("Functions should only be called with parameters of the correct type!");
# 				break;
# 			case "arr":
# 				break;
# 			default:
# 				throw new Error("Functions should only be called with parameters of the correct type!");
# 				break;
# 		}
# 		bodyToParse = bodyToParse.map(element => {
# 			if (element === parameterToReplace) {
# 				return currentArgument;
# 			} else {
# 				return element;
# 			}
# 		});
# 	}
# 	return otherInterpreter.input(bodyToParse.join(" "));
# };

    def declare_function(self):
        self.pop_token()
        params = self.read_params()
        func_params = {}
        for i in params:
            if i[0] in self.token_types['declaration']:
                func_params.update({'type': i[0], 'name': i[1]})
        self.pop_token()
        body = self.eat_block()
        return params, body

    def declare_variable(self):
        type_token = self.pop_token()
        type = str(type_token.type.name)
        name = self.eat_identifier()
        self.pop_token()
        prop = {'type': type, 'name': name, 'params': None, 'body': None}
        if type == FUNC:
            function = self.declare_function()
            prop['params'] = function[0]
            prop['body'] = function[1]
        elif type == INT or type == FLOAT:
            prop['body'] = self.expression()
        elif type == STRING:
            prop['body'] = self.expression()
        elif type == ARRAY:
            prop['body'] = self.convert_array(self.list_expression())
        elif type == BOOL:
            prop['body'] = self.expression()
        else:
            self.push_error()
        self.store_var(prop)

    def list_expression(self):
        lst = [self.comparison()]
        while self.is_token('list_sep'):
            lst.append(self.comparison())
        return lst

    def convert_array(self, expr):
        return store_array

    def conditional(self):
        if self.get_token().type == IF:
            self.pop_token()
            condition = self.comparison()
            while not condition:
                self.eat_block()
                if self.is_token('condition'):
                    if self.get_token().type == ELSE:
                        self.pop_token()
                        do = self.do_program()
                        if self.is_token('return'):
                            return do
                        self.pop_token()
                        break
                    elif self.get_token().type == ELIF:
                        self.pop_token()
                        condition = self.comparison()
                    else:
                        break
                else:
                    break
        else:
            self.eat_block()

    def do_loop(self):
        if self.get_token().type == FOR:
            self.pop_token()
            var = self.pop_token()
            self.pop_token()
            bound1 = self.convert_int(self.pop_token())
            self.pop_token()
            bound2 = self.convert_int(self.pop_token())
            loop_body = eat_block()
            child_nodes = []
            if bound1 <= bound2:
                for i in range(bound1, bound2):
                    child_nodes.append(loop_body.replace_var(lst, var, i))
            else:
                for i in range(bound2, bound1, -1):
                    child_nodes.append(loop_body.replace_var(lst, var, i))
        elif self.get_token().type == WHILE:
            pass
        else:
            print("NOT LOOP TOKEN")

    def do_program(self):
        while len(self.tokens) > 0:
            if self.is_token('declaration'):
                self.declare_variable()
            elif self.is_token('condition'):
                ret = self.conditional()
                if ret:
                    return ret
            elif self.is_token('loop'):
                self.do_loop()
            elif self.is_token('return'):
                self.eat_run_return()
            else:
                return self.expression()

class Ivy(object):

    def __init__(self):
        self.program = None
        self.argsto = None
        self.tokens = None
        self.filename = None
        self.filepath = None
        self._system = System()

    def main(self):
        arglen = len(sys.argv)
        pyfile = sys.argv[0]
        if arglen < 2:
            self.repl()
        elif arglen == 2:
            self.runFile(sys.argv[1])
        elif arglen > 2:
            self.argsto = sys.argv[2:]
            self.runFile(sys.argv[1])
        else:
            self.error('Usage', 'izy [file] [args]')

    def convert_number(self, num):
        try:
            num = int(num)
        except ValueError:
            try:
                num = float(num)
            except ValueError:
                self.error('Argument','`' + str(args[truth.index(0)]) + '` is not a valid argument.')
                quit()
        return num

    def check_args(self, args):
        truth = []
        for e,i in enumerate(args):
            if i[0] == "\'" and i[-1] == "\'":
                args[e] = str(i[1:-1])
            else:
                args[e] = self.convert_number(i)
            for ty in [int,float,str]:
                if type(i) is ty:
                    truth.append(1)
                    continue
                truth.append(0)
        if sum(truth) < len(args):
            self.error('Argument','`' + str(args[truth.index(0)]) + '` is not a valid argument.')
        return args

    def give_input(self, program):
        self.run_program(program)

    def run_program(self, program):
        self.program = program
        self._system.run_code(program)

    def runFile(self, path):
        script_dir = os.path.dirname(os.path.abspath(__file__))
        rel_path = path
        abs_file_path = os.path.join(script_dir, rel_path)
        if os.path.exists(abs_file_path):
            full_path = abs_file_path
        else:
            full_path = path
        try:
            self.filepath = full_path
            self.filename = 'file.izy'
            with open(full_path, 'r') as content_file:
                content = content_file.read()
                self.run_program(content)
        except IOError:
            self.error('File','No such file found')

    def repl(self):
        print("Izy Language REPL.")
        while True:
            getin = input("izy]> ")
            self.run_program(getin)


if __name__ == '__main__':
    for tok in all_tokens:
        exec('{name}=TokenType(\'{name}\',\'{val}\',tokens)'.format(name=str(tok[0]),val=str(tok[1])))
    system = Ivy()
    print(system.run_program("//heyyyy \n for i in 1...5 { x = 3 + 6; }"))
