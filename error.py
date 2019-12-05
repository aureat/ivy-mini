"""
***  IVY Language Interpreter
***  Error Objects

"""

class Error(Exception):
    def __init__(self, desc, name, tracestack):
        self.name = name
        self.message = desc
        self.trace = tracestack

    def get_error(self):
        error = '\nSystem Traceback\n'
        error += '(most recent activity last):\n'
        for i in self.trace.trace:
            error += str(i['type']) + '\n'
            if i['file'] != None:
                name = i['file'].name
                error += '   ' + str(i['file'].path) + '\n'
                error += f'   File {name}'
                if i['token'] != None:
                    line = i['file'].getline(i['token'].line)
                    char = i['token'].col
                    trace_beg = min(30,len(line[:char]))
                    trace_end = min(30,len(line[char:]))
                    error += f', on {line+1}:{char+1}'
                if i['name']:
                    modname = i['name']
                    error += f' in {modname}'
                if i['token'] != None:
                    error += '\n\t' + line[char-trace_beg:char+trace_end] + '\n'
                    if char >= 0: error += '\t' + " " * char + '^\n'
            elif i['filepath']:
                filepath = i['filepath']
                error += f'\t{filepath}'
        return error

    def __repr__(self):
        return self.get_error()

    __str__ = __repr__

class IvySyntaxError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'SyntaxError', trace)

class IvyParseError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'ParseError', trace)

class IvyLexerError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'LexerError', trace)

class IvyIOError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'IOError', trace)

class IvyTypeError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'TypeError', trace)
