"""
Test code generation for constrained arrays actuals passed to unconstrained
arrays parameters.
"""

from ctypes import *
from gnatllvm import build_and_load, Func

(t, ) = build_and_load(
    ['array_parameter.adb'], 'array_parameter',
    Func('array_parameter__t', argtypes=[c_int], restype=c_int),
)

print t(10)
