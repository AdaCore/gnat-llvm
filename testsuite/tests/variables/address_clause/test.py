from ctypes import *
from gnatllvm import build_and_load, Func

(Test_Address, ) = build_and_load(
    ['p.adb'], 'p',
    Func('p__test_address', argtypes=[c_int], restype=c_int),
)

for a in (1, 2, -3, 2):
    print 'Test_Address ({}) = {}'.format(a, Test_Address(a))
