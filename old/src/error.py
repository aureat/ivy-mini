"""
***  IVY Language Interpreter
***  Error Objects

"""

class Error(Exception):
    def __init__(self, desc, name, tracestack):
        self.name = name
        self.message = desc.capitalize()
        self.trace = tracestack

    def get_error(self):
        error = '\nSystem Trace (most recent activity last):\n'
        error+='...\n'
        for i in self.trace.trace:
            error += '* ' + str(i['type']) + '\n'
            if i['file'] != None:
                name = i['file'].name
                # error += '   ' + str(i['file'].path) + '\n'
                error += f'   File {name}'
                if i['token'] != None:
                    linenum = i['token'].line
                    line = i['file'].getline(linenum)
                    char = i['token'].col
                    trace_beg = min(30,len(line[:char]))
                    trace_end = min(30,len(line[char:]))
                    error += ', on {}:{}'.format(linenum+1,char)
                if i['name']:
                    modname = i['name']
                    error += f' in {modname}'
                if i['token'] != None:
                    error += '\n\t' + line[char-trace_beg:char+trace_end] + '\n'
                    if char >= 0: error += '\t' + " " * char + '^'
            elif i['filepath']:
                filepath = i['filepath']
                error += f'\t{filepath}'
            error += '\n'
        error += '> {}: {}\n'.format(self.name, self.message)
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

class IvyInterpretationError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'InterpretationError', trace)

class IvyLexerError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'LexerError', trace)

class IvyIOError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'IOError', trace)

class IvyTypeError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'TypeError', trace)

class IvyNameError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'NameError', trace)

class IvyRuntimeError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'RuntimeError', trace)

class IvyTypeError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'TypeError', trace)

class IvyAttributeError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'AttributeError', trace)

class IvyIndexError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'IndexError', trace)

class IvyCallError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'CallError', trace)

class IvyUndefinedOperation(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'UndefinedOperation', trace)

class IvyValueError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'ValueError', trace)

class IvyZeroDivisionError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'ZeroDivisionError', trace)

class IvyIterationError(Error):
    def __init__(self, desc, trace):
        super().__init__(desc, 'IterationError', trace)
