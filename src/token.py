class Token:
    def __init__(self, type=None, value=None, line=0, col=0):
        self.type = type
        self.value = value
        self.line = line
        self.col = col
        self.length = len(value) if value is not None else None

    def copypos(self):
        return (self.line, self.col)

    def __str__(self):
        return 'Token({type}, {value}, pos={line}:{col})'.format(type=self.type, value=str(self.value), line=self.line+1, col=self.col+1)

    def __repr__(self):
        return self.__str__()
