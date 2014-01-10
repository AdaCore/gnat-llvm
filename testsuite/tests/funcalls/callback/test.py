from ctypes import *
from gnatllvm import build_and_load, Func

Sum, Sub, Mul, Div = range(4)
op_to_str = '+-*/'

(Eval, ) = build_and_load(
    ['binop.adb'], 'binop',
    Func('binop__eval', argtypes=[c_char, c_int, c_int], restype=c_int),
)

for op, l, r in (
    (Sum, 1, 2),
    (Sub, 8, 4),
    (Mul, 2, 3),
    (Div, 10, 5),
):
    print '{} {} {} = {}'.format(
        l, op_to_str[op], r,
        Eval(chr(op), l, r)
    )
