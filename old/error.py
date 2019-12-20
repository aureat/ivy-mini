from objects import IvyObject

class Error(IvyObject):
    def __init__(self, trace = TraceStack(), desc, name, type=None):
        super().__init__()
        self.objdef.update({
            'error_name': name,
            'error_details': desc,
            'error_trace': trace,
            'error_type': type})

    def get_error(self):
      error = 'Error {name}: {desc}\n'.format(self.objdef['error_name'], self.objdef['error_details'])
      error += 'Traceback:\n'
      for i in self.objdef['error_trace']:
          error += 'File {filename}, line {line} by {name}'.format(i.objdef['trace_filename'], i.objdef['trace_line'], i.objdef['trace_name'])
          error += '\t' + i.objdef['trace_code']
      return error

class SyntaxError(Error):
    def __init__(self, trace, desc):
        super().__init__(self, trace, desc, 'SyntaxError')

class ParseError(Error):
    def __init__(self, trace, desc):
        super().__init__(self, trace, desc, 'ParseError')
