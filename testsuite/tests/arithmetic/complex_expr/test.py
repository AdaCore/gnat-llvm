from ctypes import *
from gnatllvm import build_and_load, Func

(complex_expr, ) = build_and_load(
    ['complex_expr.adb'], 'complex_expr',
    Func('_ada_complex_expr', argtypes=[], restype=c_int),
)

assert complex_expr(1, 41) == 1
assert complex_expr(2, 41) == 0
