"""
***  IVY Language Interpreter
***  Parser

"""
from src.tokentype import *
from src.lexer import RESERVED_KEYWORDS
from src.ast import *
from src.error import *

"""
*** Internal File Object
"""
class InternalFile:
    def __init__(self, file, tokens):
        self.file = file
        self.tokens = tokens
        self.counter = 0
        self.toklen = len(tokens)

    def next(self):
        token = self.tokens[self.counter]
        self.counter += 1
        return token

    def peek(self, amount=0):
        amount = amount if amount > 0 else 0
        return self.tokens[self.counter + amount]

"""
*** PARSER
"""

class Parser(object):
    def __init__(self, file=None, trace=None, objmachine=None):
        self.objmachine = objmachine
        self.trace = trace
        self.file = file
        self.ctoken = None

    """ Syntax Error Detection """
    def error(self, token=None, mes=None):
        mes = mes if mes is not None else 'Invalid Syntax'
        token = token if token is not None else self.ctoken
        self.trace.add('Parsing file', file=self.file.file, token=token)
        err = IvySyntaxError(mes, self.trace)
        raise err

    """ FILE and PARSER METHODS """
    def load(self, file, tokens):
        self.file = InternalFile(file, tokens)
        self.ctoken = self.file.next()

    def parse(self, file, tokens):
        self.load(file, tokens)
        res = self.program()
        if self.ctoken.type == TokenType.EOF:
            return res
        self.error(mes='EOF Error')

    def parsefile(self, file, tokens):
        self.load(file, tokens)
        return self.parse()

    """ TOKEN METHODS """
    def next_token(self):
        self.ctoken = self.file.next()

    def peek_match(self, token_type, amount=0):
        if self.file.counter < self.file.toklen:
            if self.file.tokens[self.file.counter+amount-1].type == token_type:
                return True
        return False

    def match(self, token_type):
        if self.file.counter < self.file.toklen:
            if self.ctoken.type == token_type:
                self.next_token()
                return True
        return False

    def current(self, token_type):
        if self.file.counter < self.file.toklen - 1:
            if self.ctoken.type == token_type:
                return True
        return False

    def eat(self, token_type):
        if self.ctoken.type == token_type:
            self.ctoken = self.next_token()

    def eatdef(self, token_type):
        token = self.ctoken
        if self.ctoken.type == token_type:
            self.next_token()
            return token
        else:
            self.error(mes='Unexpected token')

    """ EXPRESSIONS """

    def eat_collection(self):
        token = self.ctoken
        if self.match(TokenType.L_SQ_BRACK):
            if not self.current(TokenType.R_SQ_BRACK):
                coll = [self.expression()]
                while self.match(TokenType.COMMA):
                    if not self.current(TokenType.R_SQ_BRACK):
                        coll.append(self.expression())
                if self.match(TokenType.R_SQ_BRACK):
                    return self.objmachine.new(coll, token)
                self.error(mes='Expected a closing `]` to finish collection')
            elif self.match(TokenType.R_SQ_BRACK):
                return self.objmachine.new([], token)
        return False

    def atom(self):
        token = self.ctoken
        res = None
        do_attr = True
        if self.match(TokenType.IDENTIFIER):
            res = VariableCall(token)
        elif self.current(TokenType.L_SQ_BRACK):
            res = self.eat_collection()
        elif self.match(TokenType.PLUS):
            res = UnaryOp(token, self.atom())
        elif self.match(TokenType.MINUS):
            res=UnaryOp(token, self.atom())
        elif self.match(TokenType.TRUE):
            res = self.objmachine.fromtoken(token)
        elif self.match(TokenType.FALSE):
            res = self.objmachine.fromtoken(token)
        elif self.match(TokenType.NULL):
            res = self.objmachine.fromtoken(token)
        elif self.match(TokenType.BOOLEAN_CONST):
            res= self.objmachine.fromtoken(token)
        elif self.match(TokenType.INTEGER_CONST):
            res= self.objmachine.fromtoken(token)
        elif self.match(TokenType.FLOAT_CONST):
            res= self.objmachine.fromtoken(token)
        elif self.match(TokenType.D_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.D_QUOTE)
            res= self.objmachine.fromtoken(string)
        elif self.match(TokenType.S_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.S_QUOTE)
            res= self.objmachine.fromtoken(string)
        elif self.match(TokenType.LPAREN):
            do_attr = False
            res= self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes='Expected a closing parantheses `)` to finish expression')
        elif self.current(TokenType.FUNCTION):
            res = self.function_expression()
        else:
            self.error()
        """
            Handling calls
            ex. variable.attribute[index]().attribute[][][]()().attribute
            * Index Call [ expression ]
            * Attribute Call  .identifier
            * Function Call (expression, expression, ... , expression)
        """
        if do_attr:
            while self.current(TokenType.DOT) or self.current(TokenType.LPAREN) or self.current(TokenType.L_SQ_BRACK):
                res = self.eat_call(res)
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

    def function_expression(self):
        token = self.ctoken
        if self.match(TokenType.FUNCTION):
            if self.match(TokenType.LPAREN):
                list_decl = self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error('Expected a closing parantheses')
                if not self.current(TokenType.LBRACK):
                    self.error('Expected a block to define anonymous function')
                block = self.eat_block()
                return self.objmachine.callable(list_decl, block, token)

    """ STATEMENTS """
    def statement(self):
        token = self.ctoken
        res = self.eat_assignment()
        if not res:
            if self.match(TokenType.RETURN):
                expr = None
                if not self.peek_match(TokenType.SEMICOLON):
                    expr = self.expression()
                res = ReturnStatement(token, expr)
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
            elif self.match(TokenType.REMOVE):
                name = self.eat_id()
                res = RemoveStatement(token, name)
            else:
                res = ExpressionStatement(self.expression())
        semicolon = self.match(TokenType.SEMICOLON)
        if not semicolon:
            self.error(mes='Expected a `;` to finish statement')
        return res

    def compound_statement(self):
        statements = []
        while self.idtoken < self.toklen - 1:
            statements.append(self.statement())
        return CompoundStatement(statements)

    """ SYNTAX 'EATERS' """
    def eat_assignment(self):
        if self.current(TokenType.IDENTIFIER) and (self.peek_match(TokenType.EQUALS, 1) or self.peek_match(TokenType.DOT, 1)):
            toks = self.file.tokens
            for i in range(len(toks) - self.file.counter):
                if toks[i].type == TokenType.EQUALS:
                    break
                if toks[i].type == TokenType.SEMICOLON:
                    return False
            id = self.eat_id()
            if self.match(TokenType.EQUALS):
                expr = self.expression()
                return Assignment(id, expr)
            id = VariableCall(id)
            while self.match(TokenType.DOT):
                att = self.eat_id()
                id = AttributeAccess(id, att)
            if self.match(TokenType.EQUALS):
                expr = self.expression()
                return AttributeSet(id, att, expr)
        return False

    def eat_list_expression(self):
        getexpr = self.expression()
        expr = [getexpr] if getexpr is not None else []
        while self.match(TokenType.COMMA):
            expr.append(self.expression())
        return expr

    def eat_call(self, expr):
        token = self.ctoken
        if self.match(TokenType.DOT):
            attr = self.ctoken
            if self.match(TokenType.IDENTIFIER):
                return AttributeCall(expr, attr)
            self.error(mes='Expected an identifier to finish attribute call')
        elif self.match(TokenType.LPAREN):
            list_expr = self.eat_list_expression()
            if self.match(TokenType.RPAREN):
                return FunctionCall(expr, list_expr)
            self.error(mes="Expected a closing paranthesis ')' to finish function call")
        elif self.match(TokenType.L_SQ_BRACK):
            index = self.expression()
            if self.match(TokenType.R_SQ_BRACK):
                return IndexCall(expr, index)
            self.error(mes="Expected a closing bracket ']' to finish function call")
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
        if self.current(TokenType.IDENTIFIER):
            params = [self.eat_id()]
            while self.match(TokenType.COMMA):
                params.append(self.eat_id())
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

    def eat_block(self, single=True):
        if self.match(TokenType.LBRACK):
            block = self.program(TokenType.RBRACK)
            if not self.match(TokenType.RBRACK):
                self.error(mes='Expected a closing bracket to finish block')
            return Block(block)
        if single:
            stmt = self.statement()
            return Block([stmt])

    def eat_conditional(self):
        if self.match(TokenType.IF):
            if not self.match(TokenType.LPAREN):
                self.error(mes='Expected a paranthesis after conditional token')
            cond = self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes="Expected a closing paranthesis ')' to finish conditional expression")
            ifblock = self.eat_block()
            elseblock=None
            if self.match(TokenType.ELSE):
                if self.current(TokenType.IF):
                    elseblock = self.eat_conditional()
                else:
                    elseblock = self.eat_block()
            return Conditional(cond, ifblock, elseblock)

    def eat_while(self):
        token = self.ctoken
        if self.match(TokenType.WHILE):
            if self.match(TokenType.LPAREN):
                expr = self.expression()
                if not self.match(TokenType.RPAREN):
                    self.error(mes='Expected a closing parantheses')
                block = self.eat_block()
                return WhileLoop(token, expr, block)
            self.error(mes='Expected an opening parantheses')

    def eat_for(self): pass

    def eat_function_decl(self):
        if self.match(TokenType.FUNCTION):
            id = self.eat_id()
            if self.match(TokenType.LPAREN):
                list_decl = self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error(mes='Expected a closing parantheses')
                block = self.eat_block()
                return Assignment(id, self.objmachine.callable(list_decl, block, id, id.value))

    """ PROGRAM """
    def program(self, endtok=TokenType.EOF):
        prog = [self.eat_program()]
        while self.ctoken.type != endtok:
            prog.append(self.eat_program())
        return Program(Block(prog))

    def eat_program(self):
        if self.current(TokenType.IF):
            res = self.eat_conditional()
        elif self.current(TokenType.FUNCTION):
            res = self.eat_function_decl()
        elif self.current(TokenType.WHILE):
            res = self.eat_while()
        elif self.current(TokenType.FOR):
            res = self.eat_for()
        else:
            res = self.statement()
        return res
