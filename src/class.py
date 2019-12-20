class BaseClass:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '<Class %s at 0x%08x>' % (self.name, id(self))

    __str__ = __repr__
