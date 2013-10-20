from ctypes import *
from gnatllvm import build_and_load, Func

class ada_list_record(Structure):
    pass

ada_list = POINTER(ada_list_record)

ada_list_record._fields_ = [
    ('value', c_int),
    ('next', ada_list),
]

(length, ) = build_and_load(
    ['linked_lists.adb'], 'linked_lists',
    Func('linked_lists__length', argtypes=[ada_list], restype=c_uint),
)

def list_to_ada(l):
    result = None
    for v in reversed(l):
        result = pointer(ada_list_record(v, result))
    return result

def format_list(l):
    if len (l) > 10:
        return '[{}, ..., {}]'.format(
            ', '.join(str(item) for item in l[:5]),
            ', '.join(str(item) for item in l[-5:])
        )
    else:
        return str(l)

for l in (
    [],
    [1],
    [1, 2],
    range(10000),
):
    print 'length({}) = {}'.format(
        format_list(l),
        length(list_to_ada(l)))
