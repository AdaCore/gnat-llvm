from ctypes import *
from gnatllvm import build_and_load, Func

(Count_Digits, ) = build_and_load(
    ['count_digits.adb'], 'count_digits',
    Func('_ada_count_digits', argtypes=[c_uint32], restype=c_uint),
)

for i in (0, 1, 9, 10, 11, 99, 100, 101):
    print('Count_Digits ({}) = {}'.format(i, Count_Digits(i)))
