"""
*** Call Stack
"""

from enum import Enum

class RecordType(Enum):
    SYSTEM = 'SYSTEM'
    GLOBAL = 'GLOBAL'
    PROGRAM   = 'PROGRAM'
    FUNCTION   = 'FUNCTION'
    METHOD   = 'METHOD'

class CallStack:
    def __init__(self):
        self.records = []

    def copy(self, ar):
        if len(self.records) > 0:
            ar.members = self.peek().members
        return ar

    def push(self, ar):
        ar.init_builtin_frame()
        self.records.append(ar)

    def pop(self):
        return self.records.pop()

    def peek(self):
        return self.records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self.records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()

class Record:
    def __init__(self, name, type, depth):
        self.name = name
        self.type = type
        self.depth = depth
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members.get(key, False)

    def remove(self, key):
        self.members.pop(key)

    def get(self, key):
        return self.members.get(key)

    def init_builtin_frame(self):
        self.members.update({
            'SYSTEM_RECORD_NAME': self.name,
            'SYSTEM_RECORD_TYPE': self.type,
            'SYSTEM_RECORD_DEPTH': self.depth,
        })

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.depth,
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
