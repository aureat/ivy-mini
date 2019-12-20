"""
*** Trace Stack
"""

class SystemTrace():
    def __init__(self):
        self.trace = []

    def add(self, type, framename=None, filepath=None, file=None, token=None):
        trace = {'type': type, 'name': framename, 'filepath': filepath, 'file': file, 'token': token}
        self.trace.append(trace)

    def peek(self):
        return self.trace[-1]

    def pop(self):
        self.trace.pop()

    def __repr__(self):
        return '\n'.join([str(i) for i in self.trace])

    def clear(self):
        self.trace=[]
