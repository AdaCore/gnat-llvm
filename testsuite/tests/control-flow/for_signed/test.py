from ctypes import *
from gnatllvm import build_and_load, Func

(Fact, ) = build_and_load(
    ['fact.adb'], 'fact',
    Func('_ada_fact', argtypes=[c_uint], restype=c_uint),
)

for i in (0, 1, 2, 4, 6):
    print('Fact ({}) = {}'.format(i, Fact(i)))
