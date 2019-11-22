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


class Interpreter(object):

    def __init__(self):
        self.call_stack = CallStack()
        self.tokens = []
        self.env = {}

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
        token_types = {'declaration': [FUNCTION, INTEGER, FLOAT, STRING, ARRAY, BOOL],
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
            'file_include': [INCLUDE]}
        return self.get_token().type in token_types[type]

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
