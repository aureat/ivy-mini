"""
***  IVY Language Interpreter
***  Parser
"""

from src.token import Token
from src.tokentype import *
from src.lexer import RESERVED_KEYWORDS, ASSIGNMENT
from src.ast import *
from src.error import *
from src.objs import Function

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
    def __init__(self, trace=None):
        self.trace = trace
        self.file = None
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
        if self.current(TokenType.EOF):
            return res
        self.error(mes='EOF Error')

    """ TOKEN METHODS """
    def next_token(self):
        self.ctoken = self.file.next()

    def peek_match(self, token_type, amount=0):
        if self.file.counter <= self.file.toklen:
            if self.file.tokens[self.file.counter+amount-1].type == token_type:
                return True
        return False

    def match(self, token_type):
        if self.file.counter <= self.file.toklen:
            if self.ctoken.type == token_type:
                self.next_token()
                return True
        return False

    def current(self, token_type):
        if self.file.counter <= self.file.toklen:
            if self.ctoken.type == token_type:
                return True
        return False

    def previous(self):
        if self.file.counter <= self.file.toklen:
            return self.file.tokens[self.file.counter-1]

    def prev(self):
        if self.file.counter <= self.file.toklen:
            return self.file.tokens[self.file.counter-2]

    def eat(self, token_type):
        if self.ctoken.type == token_type:
            self.ctoken = self.next_token()

    def eatdef(self, token_type):
        token = self.ctoken
        if self.ctoken.type == token_type:
            self.next_token()
            return token
        else:
            self.error()

    """ WRAPPERS """
    def program(self):
        prog = self.compound()
        return Program(prog)

    def compound(self, endtok=TokenType.EOF, item=None):
        item = self.statement if item is None else item
        prog = [item()]
        while not self.current(endtok):
            prog.append(item())
        return Compound(prog)

    def eat_block(self, scope=True):
        if self.match(TokenType.LBRACK):
            block = self.compound(TokenType.RBRACK)
            self.eatdef(TokenType.RBRACK)
            return Block(block, scope)
        stmt = self.statement()
        return Block(stmt, scope)

    """ EXPRESSIONS """
    def eat_call(self, expr):
        reftoken = self.prev()
        token = self.ctoken
        if self.match(TokenType.DOT):
            attr = self.ctoken
            if self.match(TokenType.IDENTIFIER):
                return AttributeCall(expr, attr, reftoken)
            self.error(mes='Expected an identifier to finish attribute call')
        elif self.match(TokenType.LPAREN):
            list_expr = self.eat_list_expression()
            if self.match(TokenType.RPAREN):
                return FunctionCall(expr, list_expr, reftoken)
            self.error(mes="Expected a closing paranthesis ')' to finish call statement")
        elif self.match(TokenType.L_SQ_BRACK):
            index = self.expression()
            if self.match(TokenType.R_SQ_BRACK):
                return IndexCall(expr, index, reftoken)
            self.error(mes="Expected a closing bracket ']' to finish index call")
        return False

    def atom(self):
        token = self.ctoken
        res = None
        do_attr = True
        if self.match(TokenType.IDENTIFIER):
            res = VariableCall(token)
        elif self.match(TokenType.NEW):
            vartok = self.eat_id()
            res = VariableCall(vartok)
            reftoken = self.prev()
            self.eatdef(TokenType.LPAREN)
            list_expr = self.eat_list_expression()
            self.eatdef(TokenType.RPAREN)
            res = ConstructNew(token, res, list_expr)
        elif self.current(TokenType.L_SQ_BRACK):
            res = self.eat_collection()
        elif self.match(TokenType.PLUS):
            res = UnaryOp(token, self.atom())
        elif self.match(TokenType.MINUS):
            res=UnaryOp(token, self.atom())
        elif self.match(TokenType.TRUE):
            res = True
        elif self.match(TokenType.FALSE):
            res = False
        elif self.match(TokenType.NULL):
            res = None
        elif self.match(TokenType.INTEGER_CONST):
            res= int(token.value)
        elif self.match(TokenType.FLOAT_CONST):
            res= float(token.value)
        elif self.match(TokenType.D_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.D_QUOTE)
            res= str(string.value)
        elif self.match(TokenType.S_QUOTE):
            string = self.eatdef(TokenType.STRING_CONST)
            self.eatdef(TokenType.S_QUOTE)
            res= str(string.value)
        elif self.match(TokenType.LPAREN):
            do_attr = False
            res= self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes='Expected a closing parantheses `)` to finish expression')
        elif self.current(TokenType.FUNCTION):
            res = self.function_expression()
        else:
            return None
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
        ops = [TokenType.AND]
        while self.ctoken.type in ops:
            optok = self.ctoken
            for op in ops:
                if self.match(op):
                    binfactor = BinaryOperator(binfactor, optok, self.binfactor())
        return binfactor

    def eat_collection(self):
        token = self.ctoken
        if self.match(TokenType.L_SQ_BRACK):
            if not self.current(TokenType.R_SQ_BRACK):
                coll = [self.expression()]
                while self.match(TokenType.COMMA):
                    if not self.current(TokenType.R_SQ_BRACK):
                        coll.append(self.expression())
                if self.match(TokenType.R_SQ_BRACK):
                    return coll
                self.error(mes='Expected a closing `]` to finish collection')
            elif self.match(TokenType.R_SQ_BRACK):
                return []
        return False

    def function_expression(self):
        token = self.ctoken
        if self.match(TokenType.FUNCTION):
            if self.match(TokenType.LPAREN):
                list_decl = self.eat_list_decl()
                if not self.match(TokenType.RPAREN):
                    self.error('Expected a closing parantheses')
                if not self.current(TokenType.LBRACK):
                    self.error('Expected a block to finish function expression')
                block = self.eat_block()
                ltoken = self.previous()
                func = Function(list_decl, block, '', token, ltoken, self.trace)
                return func

    """ STATEMENTS """
    def single_statement(self):
        res = None
        token = self.ctoken
        if self.match(TokenType.RETURN):
            expr = None
            if not self.peek_match(TokenType.SEMICOLON):
                expr = self.expression()
            res = ReturnStatement(token, expr)
        elif self.match(TokenType.ASSERT):
            expr = None
            if not self.peek_match(TokenType.SEMICOLON):
                expr = self.expression()
            res = AssertStatement(token, expr)
        elif self.match(TokenType.DELETE):
            id = self.eat_id()
            res = DeleteStatement(token, id)
        elif self.match(TokenType.GLOBAL):
            id = self.eat_id()
            res = GlobalStatement(token, id)
        elif self.match(TokenType.OUTER):
            id = self.eat_id()
            res = OuterStatement(token, id)
        elif self.match(TokenType.BREAK):
            res = BreakLoop(token)
        elif self.match(TokenType.CONTINUE):
            res = ContinueLoop(token)
        if res is None: return None
        if not self.match(TokenType.SEMICOLON):
            self.error(mes='Expected a `;` to finish statement')
        return res

    def eat_while(self):
        token = self.ctoken
        if self.match(TokenType.WHILE):
            if self.match(TokenType.LPAREN):
                expr = self.expression()
                if not self.match(TokenType.RPAREN):
                    self.error(mes='Expected a closing parantheses')
                block = self.eat_block(False)
                return WhileLoop(token, expr, block)
            self.error(mes='Expected an opening parantheses')

    def eat_for(self):
        token = self.ctoken
        if self.match(TokenType.FOR):
            if self.match(TokenType.LPAREN):
                expr = self.expression()
                if not self.match(TokenType.RPAREN):
                    self.error(mes='Expected a closing parantheses')
                block = self.eat_block(False)
                return ForLoop(token, expr, block)
            self.error(mes='Expected an opening parantheses')

    def eat_conditional(self):
        if self.match(TokenType.IF):
            if not self.match(TokenType.LPAREN):
                self.error(mes='Expected a paranthesis after conditional token')
            cond = self.expression()
            if not self.match(TokenType.RPAREN):
                self.error(mes="Expected a closing paranthesis ')' to finish conditional expression")
            ifblock = self.eat_block(False)
            elseblock=None
            if self.match(TokenType.ELSE):
                if self.current(TokenType.IF):
                    elseblock = self.eat_conditional()
                else:
                    elseblock = self.eat_block(False)
            return Conditional(cond, ifblock, elseblock)

    def eat_function_decl(self):
        if self.match(TokenType.FUNCTION):
            id = self.eat_id()
            if self.match(TokenType.LPAREN):
                list_decl = self.eat_list_decl()
                self.eatdef(TokenType.RPAREN)
                block = self.eat_block()
                ltoken = self.previous()
                func = Function(list_decl, block, id.value, id, ltoken, self.trace)
                decl = FunctionDeclaration(func)
                decl.name = func.name
                return decl

    def eat_assignment(self):
        expr = self.expression()
        res = ExpressionStatement(expr) if expr is not None else None
        if self.ctoken.value in ASSIGNMENT:
            short = False
            if not self.match(TokenType.EQUALS):
                short = True
                if self.match(TokenType.ASPLUS):
                    tok = Token(TokenType.PLUS, '+')
                elif self.match(TokenType.ASMINUS):
                    tok = Token(TokenType.ASMINUS, '-')
                elif self.match(TokenType.ASMUL):
                    tok = Token(TokenType.ASMUL, '*')
                elif self.match(TokenType.ASFDIV):
                    tok = Token(TokenType.ASFDIV, '/')
                elif self.match(TokenType.ASMOD):
                    tok = Token(TokenType.ASMOD, '%')
                else:
                    self.error()
            value = self.expression()
            if short:
                value = BinaryOperator(expr, tok, value)
            if isinstance(expr, AttributeCall):
                res = AttributeSet(expr, expr.attribute, value)
            elif isinstance(expr, IndexCall):
                res = IndexSet(expr, expr.index, value)
            elif isinstance(expr, VariableCall):
                res = Assignment(expr.token, value)
            else:
                self.error()
        if res is not None and not self.match(TokenType.SEMICOLON):
            self.error(mes='Expected a `;` to finish statement')
        return res

    def eat_struct(self):
        self.match(TokenType.STRUCT)
        name = self.eat_id()
        super = None
        if self.match(TokenType.LPAREN):
            self.eatdef(TokenType.FROM)
            super = self.expression()
            self.eatdef(TokenType.RPAREN)
        struct = StructDeclaration(name, super, None)
        struct.block = self.eat_struct_block(struct)
        return struct

    def eat_struct_block(self, struct):
        if self.match(TokenType.LBRACK):
            # Ignore function declaration for struct blocks
            prog = []
            def addstmt():
                stmt = self.struct_statement(struct)
                if stmt is not None:
                    prog.append(stmt)
            addstmt()
            while not self.current(TokenType.RBRACK):
                prog.append(addstmt())
            self.eatdef(TokenType.RBRACK)
            return Block(Compound(prog))
        else:
            stmt = self.struct_statement(struct)
            return Block(stmt)

    def struct_statement(self, struct):
        res = None
        stmt = False
        if self.current(TokenType.FUNCTION):
            struct.attributes.append(MethodDeclaration(self.eat_function_decl()))
            stmt = True
        elif self.current(TokenType.STATIC):
            struct.statics.append(self.eat_static())
            stmt = True
        if not stmt:
            res = self.statement()
        return res

    def eat_static(self):
        token = self.previous()
        self.match(TokenType.STATIC)
        res = None
        if self.current(TokenType.FUNCTION):
            res = StaticDeclaration(self.eat_function_decl(), token)
        elif self.match(TokenType.IDENTIFIER):
            name = self.prev()
            self.eatdef(TokenType.EQUALS)
            value = self.expression()
            self.eatdef(TokenType.SEMICOLON)
            res = StaticDeclaration(Assignment(name, value), token)
        return res

    def statement(self):
        res = self.single_statement()
        if res is not None:
            return res
        elif self.current(TokenType.WHILE):
            res = self.eat_while()
        elif self.current(TokenType.FOR):
            res = self.eat_for()
        elif self.current(TokenType.IF):
            res = self.eat_conditional()
        elif self.current(TokenType.FUNCTION):
            res = self.eat_function_decl()
        elif self.current(TokenType.STATIC):
            res = self.eat_static()
        elif self.current(TokenType.LBRACK):
            res = self.eat_block()
        elif self.current(TokenType.STRUCT):
            res = self.eat_struct()
        else:
            res = self.eat_assignment()
        return res

    """ SYNTAX 'EATERS' """

    def eat_list_expression(self):
        getexpr = self.expression()
        expr = [getexpr] if getexpr is not None else []
        while self.match(TokenType.COMMA):
            expr.append(self.expression())
        return expr

    def eat_list_decl(self):
        params = []
        if self.current(TokenType.IDENTIFIER):
            params = [self.eat_id()]
            while self.match(TokenType.COMMA):
                params.append(self.eat_id())
            if self.match(TokenType.COMMA): pass
        return params

    def eat_id(self):
        token = self.ctoken
        if self.match(TokenType.IDENTIFIER):
            if token.value in RESERVED_KEYWORDS:
                self.error(token, 'Cannot use identifier name')
            return token
        self.error(token)
