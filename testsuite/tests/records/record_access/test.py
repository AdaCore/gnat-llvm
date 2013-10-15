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
        result = ada_list_record(v, result)
    return result

def check(l):
    assert length(list_to_ada(l)) == len(l)

check([])
check([1])
check([1, 2])
check([1] * 10000)
