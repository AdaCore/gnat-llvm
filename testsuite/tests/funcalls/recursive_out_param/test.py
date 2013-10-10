from ctypes import *
from gnatllvm import build_and_load, Func

c_uint_p = POINTER(c_uint)

(fact, ) = build_and_load(
    ['fact.adb'], 'fact',
    Func('fact', argtypes=[c_uint, c_uint_p], restype=None),
)

def compute_fact(n):
    n = c_uint(n)
    result = c_uint(0)
    fact(n, byref(result))
    return result.value

assert compute_fact(0) == 1
assert compute_fact(1) == 1
assert compute_fact(2) == 2
assert compute_fact(4) == 24
assert compute_fact(6) == 720
