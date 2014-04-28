"""
Test code generation for constrained arrays actuals passed to constrained
arrays parameters.
"""

from ctypes import *
from gnatllvm import build_and_load, Func

(t, ) = build_and_load(
    ['array_parameter.adb'], 'array_parameter',
    Func('array_parameter__t', argtypes=[], restype=c_int),
)

print t()
