from src.objects import *

class Module(IvyObject):
    def __init__(self, name, trace, interpreter):
        super().__init__(name=name, type='Module', trace=trace, interpreter=interpreter, token=None)
        self.name = name
        self.token = None
        self.objdef['istrue'] = True
        self.objdef['isnull'] = False
        self.objdef['callable'] = False
        self.objdef['indexable'] = False
        # self.attributes = {
        #     'attrget': self.newmethod(self.attrget, ['attr'], 'attrget'),
        #     'attrset': self.newmethod(self.attrset, ['attr', 'value'], 'attrset'),
        #     'attrhas': self.newmethod(self.attrhas, ['attr'], 'attrhas'),
        #     'attrdel': self.newmethod(self.attrdel, ['attr'], 'attrdel'),
        # }

    def representation(self):
        return self.newprintable("<Module %s at 0x%08x>" % (self.name, id(self)))
