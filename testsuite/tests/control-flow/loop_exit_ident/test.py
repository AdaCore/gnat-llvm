from ctypes import *
from gnatllvm import build_and_load, Func

c_uint_p = POINTER(c_uint)

(find_factors, ) = build_and_load(
    ['find_factors.adb'], 'find_factors',
    Func('find_factors', argtypes=[c_uint, c_uint_p, c_uint_p], restype=None),
)

def compute(n):
   f1, f2 = c_uint(0), c_uint(0)
   find_factors(n, byref(f1), byref(f2))
   print 'Call: fact ({}, f1, f2)'.format(n)
   print '  -> f1={}, f2={}'.format(f1.value, f2.value)

for n in (0, 1, 2, 56, 101):
    compute(n)
